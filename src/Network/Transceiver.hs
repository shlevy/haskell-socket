{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Network.Transceiver where

import Control.Monad.Catch

import Data.Monoid

class ( MonadCatch (TransceiverContext a)
      , Exception (TransceiverException a)) => Transceiver a where
  type TransceiverData a
  type TransceiverContext a :: * -> *
  type TransceiverAddress a
  type TransceiverException a

data family Listener a
data family Connection a

-- | A `StreamTransceiver` is a `Transceiver` for ordered streams of data /without/
--   message boundary preservation (like a file or
--   [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol)).
--
--  Class instances shall obey the following rules:
--
--   - `send` shall throw an exception for which `isStreamTerminatedException`
--     is true when the stream got terminated.
--   - `receive` shall return `mempty` when the other side gracefully
--     terminated the connection.
--
class (Transceiver a, Data.Monoid.Monoid (TransceiverData a), Eq (TransceiverData a)) => StreamTransceiver a where
  -- | Send a chunk of data.
  send    :: Connection a -> TransceiverData a -> (TransceiverContext a) ()
  -- | Receive a chunk of data.
  receive :: Connection a -> (TransceiverContext a) (TransceiverData a)
  isStreamTerminatedException :: a -> TransceiverException a -> Bool

class DatagramTransceiver a where
  sendDatagram    :: Connection a -> TransceiverData a -> (TransceiverContext a) ()
  sendDatagramTo  :: a -> (TransceiverData a, TransceiverAddress a) -> (TransceiverContext a) ()
  receiveDatagram :: Connection a -> (TransceiverContext a) (TransceiverData a)
  receiveDatagramFrom :: Listener a -> (TransceiverContext a) (TransceiverData a, TransceiverAddress a)

class Transceiver a => ConnectableTransceiver a where
  connect :: a -> TransceiverAddress a -> (TransceiverContext a) (Connection a)

class Transceiver a => ListenableTransceiver a where
  type ListeningOptions a
  listen  :: a -> ListeningOptions a -> (TransceiverContext a) (Listener a)
