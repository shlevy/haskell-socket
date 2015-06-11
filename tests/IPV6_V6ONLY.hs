{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bits
import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import System.Socket
import System.Socket.Family.INET
import System.Socket.Family.INET6
import System.Exit

main :: IO ()
main = do 
  t0001
  t0002

t0001 :: IO ()
t0001 = 
  bracket
    ( do
        server <- socket                              `onException` p 0 :: IO (Socket INET6 DGRAM UDP)
        client <- socket                              `onException` p 1 :: IO (Socket INET  DGRAM UDP)
        return (server, client)
    )
    (\(server,client)-> do
        close server                                  `onException` p 2
        close client                                  `onException` p 3
    )
    (\(server,client)-> do
        setSockOpt server (IPV6_V6ONLY True)          `onException` p 4
        bind server (SockAddrIn6 7777 0 in6addrANY 0) `onException` p 5

        threadDelay 1000000 -- wait for the listening socket being set up
        sendTo client "PING" mempty (SockAddrIn 7777 inaddrLOOPBACK)
                                                      `onException` p 6
        eith <- race
          ( recvFrom server 4096 mempty `onException` p 7 >> return () )
          ( threadDelay 1000000 )
        case eith of
          Left  () -> e 8        -- we didn't expect receiving a msg
          Right () -> return ()  -- timeout is the expected behaviour
    )
  where
    e i  = error ("t0001." ++ show i)
    p i  = print ("t0001." ++ show i)

t0002 :: IO ()
t0002 = 
  bracket
    ( do
        server <- socket                              `onException` p 0 :: IO (Socket INET6 DGRAM UDP)
        client <- socket                              `onException` p 1 :: IO (Socket INET  DGRAM UDP)
        return (server, client)
    )
    (\(server,client)-> do
        close server                                  `onException` p 2
        close client                                  `onException` p 3
    )
    (\(server,client)-> do
        setSockOpt server (IPV6_V6ONLY False)         `onException` p 4
        bind server (SockAddrIn6 7778 0 in6addrANY 0) `onException` p 5

        threadDelay 1000000 -- wait for the listening socket being set up
        sendTo client "PING" mempty (SockAddrIn 7778 inaddrLOOPBACK)
                                                      `onException` p 6
        eith <- race
          ( recvFrom server 4096 mempty `onException` p 7 >> return ())
          ( threadDelay 1000000 )
        case eith of
          Left  () -> return ()  -- we received the expected msg
          Right () -> e 8        -- timeout occured
    )
  where
    e i  = error ("t0002." ++ show i)
    p i  = print ("t0002." ++ show i)
