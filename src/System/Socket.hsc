{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module System.Socket
  ( -- * Operations
   -- ** socket
     socket
   -- ** bind
   , bind
   -- ** listen
   , listen
   -- ** accept
   , accept
   -- ** connect
   , connect
   -- ** send
   , send
   -- ** recv
   , recv
      -- ** close
   , close
  -- * Sockets
  , Socket (..)
  -- ** Address Families
  , AddressFamily (..)
  -- *** AF_UNIX
  , AF_UNIX
  , SockAddrUn
  -- *** AF_INET
  , AF_INET
  , SockAddrIn (..)
  -- *** AF_INET6
  , AF_INET6
  , SockAddrIn6 (..)
  -- ** Types
  , Type (..)
  -- *** SOCK_STREAM
  , SOCK_STREAM
  -- *** SOCK_DGRAM
  , SOCK_DGRAM
  -- *** SOCK_SEQPACKET
  , SOCK_SEQPACKET
  -- ** Protocols
  , Protocol  (..)
  -- *** IPPROTO_UDP
  , IPPROTO_UDP
  -- *** IPPROTO_TCP
  , IPPROTO_TCP

  , localhost
  , SocketException (..)
  ) where

import Control.Exception
import Control.Monad
import Control.Concurrent.MVar

import Data.Function
import Data.Word
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import GHC.Conc (threadWaitReadSTM, threadWaitWriteSTM, atomically, closeFdWith)

import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import System.IO
import System.Posix.Types
import System.Posix.Internals (setNonBlockingFD)

#include "sys/types.h"
#include "sys/socket.h"
#include "netinet/in.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | A generic socket type. Also see `socket` for details.
--
--   The socket is just an `Control.Concurrent.MVar.MVar`-wrapped file descriptor.
--   It is exposed in order to make this library easily extensible, but it is
--   usually not necessary nor advised to work directly on the file descriptor.
--   If you do, the following rules must be obeyed:
--
--   - Make sure not to deadlock. Use `Control.Concurrent.MVar.withMVar` or similar.
--   - The lock __must not__ be held during a blocking call. This would make it impossible
--     to send and receive simultaneously or to close the socket.
--   - The lock __must__ be held when calling operations that use the file descriptor.
--     Otherwise the socket might get closed or even reused by another
--     thread/capability which might result in reading from or writing
--     totally different connection. This is a security nightmare!
--   - The socket is non-blocking and all the code relies on that assumption.
--     You need to use GHC's eventing mechanism primitives to block until
--     something happens. The former rules forbid to use `GHC.Conc.threadWaitRead` as it
--     does not seperate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket d t p
      = Socket (MVar Fd)

data AF_UNIX
data AF_INET
data AF_INET6

data SOCK_STREAM
data SOCK_DGRAM
data SOCK_SEQPACKET

data IPPROTO_UDP
data IPPROTO_TCP

data IP_SOCKOPT
   = IP_MTU
   | IP_TTL

data IP6_SOCKOPT

data TCP_SOCKOPT
   = TCP_CONGESTION
   | TCP_CORK
   | TCP_DEFER_ACCEPT
   | TCP_INFO
   | TCP_KEEPCNT
   | TCP_KEEPIDLE
   | TCP_KEEPINTVL
   | TCP_MAXSEG
   | TCP_NODELAY
   | TCP_QUICKACK
   | TCP_SYNCNT
   | TCP_USER_TIMEOUT
   | TCP_WINDOW_CLAMP

setSockOptAddressFamily :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> SockOpt f -> IO ()
setSockOptAddressFamily
  = undefined

setSockOptProtocol  :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> SockOpt p -> IO ()
setSockOptProtocol 
  = undefined

class (Storable (SockAddr d)) => AddressFamily d where
  type SockAddr d
  addressFamilyNumber :: d -> CInt

type family SockOpt o :: *
type instance SockOpt AF_INET     = IP_SOCKOPT
type instance SockOpt AF_INET6    = IP6_SOCKOPT
type instance SockOpt IPPROTO_TCP = TCP_SOCKOPT

instance AddressFamily AF_UNIX where
  type SockAddr AF_UNIX = SockAddrUn
  addressFamilyNumber _ = (#const AF_UNIX)

instance AddressFamily AF_INET where
  type SockAddr AF_INET = SockAddrIn
  addressFamilyNumber _ = (#const AF_INET)

instance AddressFamily AF_INET6 where
  type SockAddr AF_INET6 = SockAddrIn6
  addressFamilyNumber _ = (#const AF_INET6)

class Type t where
  typeNumber :: t -> CInt

instance Type SOCK_STREAM where
  typeNumber _ = (#const SOCK_STREAM)

instance Type SOCK_DGRAM where
  typeNumber _ = (#const SOCK_DGRAM)

instance Type SOCK_SEQPACKET where
  typeNumber _ = (#const SOCK_SEQPACKET)

class Protocol  p where
  protocolNumber :: p -> CInt

instance Protocol  IPPROTO_TCP where
  protocolNumber _ = (#const IPPROTO_TCP)



-- | Creates a new socket.
--
--   Whereas the underlying POSIX socket function takes 3 parameters, this library
--   encodes this information in the type variables. This rules out several
--   kinds of errors and escpecially simplifies the handling of addresses (by using
--   associated type families). Examples:
--
--   > -- create a IPv4-UDP-datagram socket
--   > sock <- socket :: IO (Socket AF_INET SOCK_DGRAM IPPROTO_UDP)
--   > -- create a IPv6-TCP-streaming socket
--   > sock6 <- socket :: IO (Socket AF_INET6 SOCK_STREAM IPPROTO_TCP)
--
--     - This operation sets up a finalizer that automatically closes the socket
--       when the garbage collection decides to collect it. This is just a
--       fail-safe. You might still run out of file descriptors as there's
--       no guarantee about when the finalizer is run. You're advised to
--       manually `close` the socket when it's no longer needed.
--     - This operation configures the socket non-blocking to work seamlessly
--       with the runtime system's event notification mechanism.
--     - This operation can safely deal with asynchronous exceptions without
--       leaking file descriptors.
--     - This operation throws `SocketException`s:
--
--        [@EAFNOSUPPORT@]    The socket domain is not supported.
--        [@EMFILE@]          The process is out file descriptors.
--        [@ENFILE@]          The system is out file descriptors.
--        [@EPROTONOSUPPORT@] The socket protocol is not supported (for this socket domain).
--        [@EPROTOTYPE@]      The socket type is not supported by the protocol.
--        [@EACCES@]          The process is lacking necessary privileges.
--        [@ENOMEM@]          Insufficient memory.
socket :: (AddressFamily f, Type t, Protocol  p) => IO (Socket f t p)
socket = socket'
 where
   socket' :: forall f t p. (AddressFamily f, Type t, Protocol  p) => IO (Socket f t p)
   socket'  = do
     bracketOnError
       -- Try to acquire the socket resource. This part has exceptions masked.
       ( c_socket (addressFamilyNumber (undefined :: f)) (typeNumber (undefined :: t)) (protocolNumber (undefined :: p)) )
       -- On failure after the c_socket call we try to close the socket to not leak file descriptors.
       -- If closing fails we cannot really do something about it. We tried at least.
       -- This part has exceptions masked as well. c_close is an unsafe FFI call.
       ( \fd-> when (fd >= 0) (c_close fd >> return ()) )
       -- If an exception is raised, it is reraised after the socket has been closed.
       -- This part has async exceptions unmasked (via restore).
       ( \fd-> if fd < 0 then do
                getErrno >>= throwIO . SocketException
              else do
                -- setNonBlockingFD calls c_fcntl_write which is an unsafe FFI call.
                let Fd s = fd in setNonBlockingFD s True
                mfd <- newMVar fd
                let s = Socket mfd
                mkWeakMVar mfd (close s)
                return s
       )

-- | Bind a socket to an address.
--
--   - Calling `bind` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - This operation throws `SocketException`s (consult your Posix manpages for more protocol specific exceptions):
--
--     [@EADDRINUSE@]     The address is in use.
--     [@EADDRNOTAVAIL@]  The address is not available.
--     [@EAFNOSUPPORT@]   The address is domain invalid (should not occur due to type safety).
--     [@EALREADY@]       An assignment request is already in progress.
--     [@EBADF@]          Not a valid file descriptor (only after socket has been closed).
--     [@EINPROGRESS@]    The assignment shall be performed asychronously.
--     [@EINVAL@]         Socket is already bound and cannot be re-bound or the socket has been shut down.
--     [@ENOBUFS@]        Insufficient resources.
--     [@ENOTSOCK@]       The file descriptor is not a socket (should be impossible).
--     [@EOPNOTSUPP@]     The socket type does not support binding.
--     [@EACCES@]         The address is protected and the process is lacking permission.
--     [@EINVAL@]         Address length does not match address family (should be impossible).
--     [@EISCONN@]        The socket is already connected.
--     [@ELOOP@]          More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the pathname in address.
--     [@ENAMETOOLONG@]   The length of a pathname exceeds {PATH_MAX}, or pathname resolution of a symbolic link produced an intermediate result  with  a  length  that  exceeds {PATH_MAX}.
bind :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> SockAddr f -> IO ()
bind (Socket ms) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    i <- withMVar ms $ \s-> do
      c_bind s (castPtr addrPtr :: Ptr ()) (sizeOf addr)
    if i < 0 then do
      getErrno >>= throwIO . SocketException
    else do
      return ()

-- | Accept connections on a connection-mode socket.
--
--   - The second parameter is called /backlog/ and sets a limit on how many
--     unaccepted connections the socket implementation shall queue. A value
--     of @0@ leaves the decision to the implementation.
--   - Calling `listen` on a `close`d socket throws @EBADF@ even if the former file descriptor has been reassigned.
--   - This operation throws `SocketException`s:
--
--     [@EBADF@]          Not a valid file descriptor (only after socket has been closed).
--     [@EDESTADDRREQ@]   The socket is not bound and the protocol does not support listening on an unbound socket.
--     [@EINVAL@]         The socket is already connected or has been shut down.
--     [@ENOTSOCK@]       The file descriptor is not a socket (should be impossible).
--     [@EOPNOTSUPP@]     The protocol does not support listening.
--     [@EACCES@]         The process is lacking privileges.
--     [@ENOBUFS@]        Insufficient resources.
listen :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> Int -> IO ()
listen (Socket ms) backlog = do
  i <- withMVar ms $ \s-> do
    c_listen s backlog
  if i < 0 then do
    getErrno >>= throwIO . SocketException
  else do
    return ()

-- | Accept a new connection.
--
--   - Calling this operation on an already closed socket always throws `EBADF` even if the former descriptor has been reassigned.
--   - This operation configures the new socket non-blocking (TODO: use `accept4` if available).
--   - This operation sets up a finalizer for the new socket that automatically closes the socket
--     when the garbage collection decides to collect it. This is just a
--     fail-safe. You might still run out of file descriptors as there's
--     no guarantee about when the finalizer is run. You're advised to
--     manually `close` the socket when it's no longer needed.
--   - This operation catches @EAGAIN@, @EWOULDBLOCK@ and @EINTR@ internally and retries automatically.
--   - This operation throws `SocketException`s:
--
--     [@EBADF@]        Not a valid file descriptor (only after the socket has been closed).
--     [@ECONNABORTED@] A connection has been aborted.
--     [@EINVAL@]       The socket is not accepting/listening.
--     [@EMFILE@]       The process is out file descriptors.
--     [@ENFILE@]       The system is out file descriptors.
--     [@ENOBUFS@]      No buffer space available.
--     [@ENOMEM@]       Out of memory.
--     [@ENOSOCK@]      Not a valid socket descriptor (should be impossible).
--     [@EOPNOTSUPP@]   The socket type does not support accepting connections.
--     [@EPROTO@]       Generic protocol error.
accept :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> IO (Socket f t p, SockAddr f)
accept s@(Socket mfd) = acceptWait
  where
    acceptWait :: forall f t p. (AddressFamily f, Type t, Protocol  p) => IO (Socket f t p, SockAddr f)
    acceptWait = do
      -- This is the blocking operation waiting for an event.
      threadWaitReadMVar mfd `onException` throwIO (SocketException eBADF)
      -- We mask asynchronous exceptions during this critical section.
      msa <- withMVarMasked mfd $ \fd-> do
        -- Allocate local (!) memory for the address.
        alloca $ \addrPtr->
          -- Oh Haskell, my beauty!
          fix $ \retry-> do
            ft <- c_accept fd (castPtr addrPtr :: Ptr ()) (sizeOf (undefined :: SockAddr f))
            if ft < 0 then do
              e <- getErrno
              if e == eWOULDBLOCK || e == eAGAIN 
                then return Nothing
                else if e == eINTR
                  -- On EINTR it is good practice to just retry.
                  then retry
                  else throwIO (SocketException e)
            -- This is the critical section: We got a valid descriptor we have not yet returned.
            else do 
              -- setNonBlockingFD calls c_fcntl_write which is an unsafe FFI call.
              let Fd t = ft in setNonBlockingFD t True -- FIXME: throws exception
              -- This peek operation might be a little expensive, but I don't see an alternative.
              addr <- peek addrPtr :: IO (SockAddr f)
              -- newMVar is guaranteed to be not interruptible.
              mft <- newMVar ft
              -- Register a finalizer on the new socket.
              mkWeakMVar mft (close (Socket mft `asTypeOf` s))
              return (Just (Socket mft, addr))
      -- If msa is Nothing we got EAGAIN or EWOULDBLOCK and retry after the next event.
      case msa of
        Just sa -> return sa
        Nothing -> acceptWait

-- | Closes a socket.
-- In contrast to the POSIX close this operation is idempotent.
-- On EINTR the close operation is retried.
-- On EBADF an error is thrown as this should be impossible according to the library's design.
-- On EIO an error is thrown.
close :: (AddressFamily f, Type t, Protocol  p) => Socket f t p -> IO ()
close (Socket mfd) = do
  modifyMVarMasked_ mfd $ \fd-> do
    if fd < 0 then do
      return fd
    else do
      -- closeFdWith does not throw even on invalid file descriptors.
      -- It just assures no thread is blocking on the fd anymore and then executes the IO action.
      closeFdWith
        -- The c_close operation may (according to Posix documentation) fails with EINTR or EBADF or EIO.
        -- EBADF: Should be ruled out by the library's design.
        -- EINTR: It is best practice to just retry the operation what we do here.
        -- EIO: Only occurs when filesystem is involved (?).
        -- Conclusion: Our close should never fail. If it does, something is horribly wrong.
        ( const $ fix $ \loop-> do
            i <- c_close fd
            if i < 0 then do
              e <- getErrno
              if e == eINTR 
                then loop
                else throwIO (SocketException e)
            else return ()
        ) fd
      -- When we arrive here, no exception has been thrown and the descriptor has been closed.
      -- We put an invalid file descriptor into the MVar.
      return (-1)

connect :: (AddressFamily d, Type t, Protocol  p) => Socket d t p -> SockAddr d -> IO ()
connect (Socket ms) addr = do
  alloca $ \addrPtr-> do
    poke addrPtr addr
    i <- withMVar ms $ \s-> do
      c_connect s (castPtr addrPtr :: Ptr ()) (sizeOf addr)
    if i < 0 then do
      getErrno >>= throwIO . SocketException
    else do
      return ()

recv     :: Socket d t p -> Int -> IO BS.ByteString
recv (Socket mfd) bufSize = recvWait
  where
    recvWait = do
      threadWaitReadMVar mfd
      mbs <- withMVarMasked mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        ptr <- mallocBytes bufSize
        fix $ \loop-> do
          i <- c_recv fd ptr bufSize 0
          if (i < 0) then do
            e <- getErrno
            if e == eWOULDBLOCK || e == eAGAIN 
              then do
                -- At this exit we need to free the pointer manually.
                free ptr
                return Nothing
            else if e == eINTR
              then loop
              else do
                -- At this exit we need to free the pointer manually as well.
                free ptr
                throwIO (SocketException e)
          else do
            -- Send succeeded, generate a ByteString.
            -- From now on the ByteString takes care of freeing the pointer somewhen in the future.
            -- The resulting ByteString might be shorter than the malloced buffer.
            -- This is a fair tradeoff as otherwise we had to create a fresh copy.
            bs <- BS.unsafePackMallocCStringLen (ptr, i)
            return (Just bs)
      -- We cannot loop from within the block above, because this would keep the MVar locked.
      case mbs of
        Nothing -> recvWait
        Just bs -> return bs

send     :: Socket d t p -> BS.ByteString -> IO Int
send (Socket mfd) bs = sendWait
  where
    sendWait = do
      threadWaitWriteMVar mfd
      bytesSend <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        BS.unsafeUseAsCStringLen bs $ \(ptr,len)->
          fix $ \loop-> do
            i <- c_send fd ptr len 0
            if (i < 0) then do
              e <- getErrno
              if e == eWOULDBLOCK || e == eAGAIN 
                then return i
              else if e == eINTR
                then loop
                else throwIO (SocketException e)
            -- Send succeeded. Return the bytes send.
            else return i
      -- We cannot loop from within the block above, because this would keep the MVar locked.
      if bytesSend < 0
        then sendWait
        else return bytesSend

data SockAddrUn
   = SockAddrUn
     {

     } deriving (Eq, Ord, Show)

data SockAddrIn
   = SockAddrIn
     { sinPort      :: Word16
     , sinAddr      :: BS.ByteString
     } deriving (Eq, Ord, Show)

data SockAddrIn6
   = SockAddrIn6
     { sin6Port      :: Word16
     , sin6Flowinfo  :: Word32
     , sin6Addr      :: BS.ByteString
     , sin6ScopeId   :: Word32
     } deriving (Eq, Ord, Show)

localhost :: SockAddrIn6
localhost =
  SockAddrIn6
  { sin6Port     = 5555
  , sin6Flowinfo = 0
  , sin6Addr     = BS.pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1]
  , sin6ScopeId  = 0
  }

foreign import ccall safe "sys/socket.h socket"
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall safe "unistd.h close"
  c_close   :: Fd -> IO CInt

foreign import ccall safe "sys/socket.h bind"
  c_bind    :: Fd -> Ptr () -> Int -> IO CInt

foreign import ccall safe "sys/socket.h connect"
  c_connect :: Fd -> Ptr () -> Int -> IO CInt

foreign import ccall safe "sys/socket.h accept"
  c_accept  :: Fd -> Ptr () -> Int -> IO Fd

foreign import ccall safe "sys/socket.h listen"
  c_listen  :: Fd -> Int -> IO CInt

foreign import ccall safe "sys/socket.h send"
  c_send  :: Fd -> Ptr CChar -> Int -> Int -> IO Int

foreign import ccall safe "sys/socket.h recv"
  c_recv  :: Fd -> Ptr CChar -> Int -> Int -> IO Int

instance Storable SockAddrUn where
  sizeOf = undefined
  alignment = undefined
  peek = undefined
  poke = undefined

instance Storable SockAddrIn where
  sizeOf    _ = (#size struct sockaddr_in)
  alignment _ = (#alignment struct sockaddr_in)
  peek ptr    = do
    ph  <- peekByteOff       (sin_port ptr)  0  :: IO Word8
    pl  <- peekByteOff       (sin_port ptr)  1  :: IO Word8
    a   <- BS.packCStringLen (sin_addr ptr,  4) :: IO BS.ByteString
    return (SockAddrIn (fromIntegral ph * 256 + fromIntegral pl) a)
    where
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
  poke ptr (SockAddrIn p a) = do
    poke        (sin_family   ptr) ((#const AF_INET) :: Word16)
    pokeByteOff (sin_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    BS.unsafeUseAsCString a $ \a'-> do
      copyBytes (sin_addr ptr) a' (min 4 $ BS.length a)-- copyBytes dest from count
    where
      sin_family   = (#ptr struct sockaddr_in, sin_family)
      sin_port     = (#ptr struct sockaddr_in, sin_port)
      sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)

instance Storable SockAddrIn6 where
  sizeOf    _ = (#size struct sockaddr_in6)
  alignment _ = (#alignment struct sockaddr_in6)
  peek ptr    = do
    f   <- peek              (sin6_flowinfo ptr) :: IO Word32
    ph  <- peekByteOff       (sin6_port ptr)  0  :: IO Word8
    pl  <- peekByteOff       (sin6_port ptr)  1  :: IO Word8
    a   <- BS.packCStringLen (sin6_addr ptr, 16) :: IO BS.ByteString
    s   <- peek              (sin6_scope_id ptr) :: IO Word32
    return (SockAddrIn6 (fromIntegral ph * 256 + fromIntegral pl) f a s)
    where
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)
  poke ptr (SockAddrIn6 p f a s) = do
    poke        (sin6_family   ptr) ((#const AF_INET6) :: Word16)
    poke        (sin6_flowinfo ptr) f
    poke        (sin6_scope_id ptr) s
    pokeByteOff (sin6_port     ptr)  0 (fromIntegral $ rem (quot p 256) 256 :: Word8)
    pokeByteOff (sin6_port     ptr)  1 (fromIntegral $ rem       p      256 :: Word8)
    BS.unsafeUseAsCString a $ \a'-> do
      copyBytes (sin6_addr ptr) a' (min 16 $ BS.length a)-- copyBytes dest from count
    where
      sin6_family   = (#ptr struct sockaddr_in6, sin6_family)
      sin6_flowinfo = (#ptr struct sockaddr_in6, sin6_flowinfo)
      sin6_scope_id = (#ptr struct sockaddr_in6, sin6_scope_id)
      sin6_port     = (#ptr struct sockaddr_in6, sin6_port)
      sin6_addr     = (#ptr struct in6_addr, s6_addr) . (#ptr struct sockaddr_in6, sin6_addr)

-- helpers for threadsafe event registration on file descriptors

threadWaitReadMVar :: MVar Fd -> IO ()
threadWaitReadMVar mfd = do
  withMVar mfd threadWaitReadSTM >>= atomically . fst

threadWaitWriteMVar :: MVar Fd -> IO ()
threadWaitWriteMVar mfd = do
  withMVar mfd threadWaitWriteSTM >>=  atomically . fst

-- exceptions
newtype SocketException = SocketException Errno
  deriving Typeable

instance Exception SocketException

instance Show SocketException where
  show (SocketException e) = "SocketException " ++ strerr e
    where 
      strerr errno
        | errno == eOK             = "EOK"
        | errno == e2BIG           = "E2BIG"
        | errno == eACCES          = "EACCES"
        | errno == eADDRINUSE      = "EADDRINUSE"
        | errno == eADDRNOTAVAIL   = "EADDRNOTAVAIL"
        | errno == eADV            = "EADV"
        | errno == eAFNOSUPPORT    = "EAFNOSUPPORT"
        | errno == eAGAIN          = "EAGAIN"
        | errno == eALREADY        = "EALREADY"
        | errno == eBADF           = "EBADF"
        | errno == eBADMSG         = "EBADMSG"
        | errno == eBADRPC         = "EBADRPC"
        | errno == eBUSY           = "EBUSY"
        | errno == eCHILD          = "ECHILD"
        | errno == eCOMM           = "ECOMM"
        | errno == eCONNABORTED    = "ECONNABORTED"
        | errno == eCONNREFUSED    = "ECONNREFUSED"
        | errno == eCONNRESET      = "ECONNRESET"
        | errno == eDEADLK         = "EDEADLK"
        | errno == eDESTADDRREQ    = "EDESTADDRREQ"
        | errno == eDIRTY          = "EDIRTY"
        | errno == eDOM            = "EDOM"
        | errno == eDQUOT          = "EDQUOT"
        | errno == eEXIST          = "EEXIST"
        | errno == eFAULT          = "EFAULT"
        | errno == eFBIG           = "EFBIG"
        | errno == eFTYPE          = "EFTYPE"
        | errno == eHOSTDOWN       = "EHOSTDOWN"
        | errno == eHOSTUNREACH    = "EHOSTUNREACH"
        | errno == eIDRM           = "EIDRM"
        | errno == eILSEQ          = "EILSEQ"
        | errno == eINPROGRESS     = "EINPROGRESS"
        | errno == eINTR           = "EINTR"
        | errno == eINVAL          = "EINVAL"
        | errno == eIO             = "EIO"
        | errno == eISCONN         = "EISCONN"
        | errno == eISDIR          = "EISDIR"
        | errno == eLOOP           = "ELOOP"
        | errno == eMFILE          = "EMFILE"
        | errno == eMLINK          = "EMLINK"
        | errno == eMSGSIZE        = "EMSGSIZE"
        | errno == eMULTIHOP       = "EMULTIHOP"
        | errno == eNAMETOOLONG    = "ENAMETOOLONG"
        | errno == eNETDOWN        = "ENETDOWN"
        | errno == eNETRESET       = "ENETRESET"
        | errno == eNETUNREACH     = "ENETUNREACH"
        | errno == eNFILE          = "ENFILE"
        | errno == eNOBUFS         = "ENOBUFS"
        | errno == eNODATA         = "ENODATA"
        | errno == eNODEV          = "ENODEV"
        | errno == eNOENT          = "ENOENT"
        | errno == eNOEXEC         = "ENOEXEC"
        | errno == eNOLCK          = "ENOLCK"
        | errno == eNOLINK         = "ENOLINK"
        | errno == eNOMEM          = "ENOMEM"
        | errno == eNOMSG          = "ENOMSG"
        | errno == eNONET          = "ENONET"
        | errno == eNOPROTOOPT     = "ENOPROTOOPT"
        | errno == eNOSPC          = "ENOSPC"
        | errno == eNOSR           = "ENOSR"
        | errno == eNOSTR          = "ENOSTR"
        | errno == eNOSYS          = "ENOSYS"
        | errno == eNOTBLK         = "ENOTBLK"
        | errno == eNOTCONN        = "ENOTCONN"
        | errno == eNOTDIR         = "ENOTDIR"
        | errno == eNOTEMPTY       = "ENOTEMPTY"
        | errno == eNOTSOCK        = "ENOTSOCK"
        | errno == eNOTTY          = "ENOTTY"
        | errno == eNXIO           = "ENXIO"
        | errno == eOPNOTSUPP      = "EOPNOTSUPP"
        | errno == ePERM           = "EPERM"
        | errno == ePFNOSUPPORT    = "EPFNOSUPPORT"
        | errno == ePIPE           = "EPIPE"
        | errno == ePROCLIM        = "EPROCLIM"
        | errno == ePROCUNAVAIL    = "EPROCUNAVAIL"
        | errno == ePROGMISMATCH   = "EPROGMISMATCH"
        | errno == ePROGUNAVAIL    = "EPROGUNAVAIL"
        | errno == ePROTO          = "EPROTO"
        | errno == ePROTONOSUPPORT = "EPROTONOSUPPORT"
        | errno == ePROTOTYPE      = "EPROTOTYPE"
        | errno == eRANGE          = "ERANGE"
        | errno == eREMCHG         = "EREMCHG"
        | errno == eREMOTE         = "EREMOTE"
        | errno == eROFS           = "EROFS"
        | errno == eRPCMISMATCH    = "ERPCMISMATCH"
        | errno == eRREMOTE        = "ERREMOTE"
        | errno == eSHUTDOWN       = "ESHUTDOWN"
        | errno == eSOCKTNOSUPPORT = "ESOCKTNOSUPPORT"
        | errno == eSPIPE          = "ESPIPE"
        | errno == eSRCH           = "ESRCH"
        | errno == eSRMNT          = "ESRMNT"
        | errno == eSTALE          = "ESTALE"
        | errno == eTIME           = "ETIME"
        | errno == eTIMEDOUT       = "ETIMEDOUT"
        | errno == eTOOMANYREFS    = "ETOOMANYREFS"
        | errno == eTXTBSY         = "ETXTBSY"
        | errno == eUSERS          = "EUSERS"
        | errno == eWOULDBLOCK     = "EWOULDBLOCK"
        | errno == eXDEV           = "EXDEV"
        | otherwise                = let Errno i = errno
                                     in  show i
