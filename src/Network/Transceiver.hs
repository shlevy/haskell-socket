{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Network.Transceiver where

import Control.Monad.Catch

class ( MonadCatch (TransceiverContext a)
      , Exception (TransceiverException a)) => Transceiver a where
  type TransceiverData a
  type TransceiverContext a :: * -> *
  type TransceiverException a

-- | A `StreamTransceiver` is a `Transceiver` for ordered streams of data /without/
--   message boundary preservation (like a file or
--   [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol)).
--
--  Class instances shall obey the following rules:
--
--   - `send` shall throw an exception for which `isStreamTerminatedException`
--     is true when the other side gracefully terminated the connection.
--   - `receive` shall return `mempty` when the other side gracefully
--     terminated the connection.
--
class (Transceiver a, Monoid (TransceiverData a), Eq (TransceiverData a)) => StreamTransceiver a where
  -- | Send a chunk of data.
  send    :: a -> TransceiverData a -> (TransceiverContext a) ()
  -- | Receive a chunk of data.
  receive :: a -> (TransceiverContext a) (TransceiverData a)
  isStreamTerminatedException :: a -> TransceiverException a -> Bool
