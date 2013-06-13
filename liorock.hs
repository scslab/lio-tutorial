{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import qualified Control.Exception as IO
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import Data.Monoid
import Network (PortID(..), HostName, PortNumber)
import qualified Network as IO
import System.IO (BufferMode(..))
import qualified System.IO as IO

import LIO
import LIO.Concurrent
import LIO.Concurrent.LMVar
import LIO.BlessIO.TCB
import LIO.DCLabel
import LIO.TCB
import LIO.Privs.TCB

port :: PortID
port = PortNumber 1617

--
-- Some DC functions for accessing IO abstractions
--

type Handle = Blessed DCLabel IO.Handle

hPutStrLnP :: PrivDesc l p =>
   Priv p -> Blessed l IO.Handle -> String -> LIO l ()
hPutStrLnP p = blessPTCB IO.hPutStrLn p

hPutStrLn :: Label l => Blessed l IO.Handle -> String -> LIO l ()
hPutStrLn h = blessTCB IO.hPutStrLn h

hGetLine :: Label l => Blessed l IO.Handle -> LIO l String
hGetLine h = blessTCB IO.hGetLine h

hSetBufferingP :: PrivDesc l p =>
                  Priv p -> Blessed l IO.Handle -> BufferMode -> LIO l ()
hSetBufferingP p = blessPTCB IO.hSetBuffering p

hCloseP :: PrivDesc l p => Priv p -> Blessed l IO.Handle -> LIO l ()
hCloseP p = blessPTCB IO.hClose p

type Socket = Blessed DCLabel IO.Socket

acceptP :: DCPriv -> Socket -> DC (Handle, HostName, PortNumber)
acceptP p s = do
  (ioh, name, port) <- blessPTCB IO.accept p s
  let net = principal $ S8.pack $ "net:" ++ name ++ ":" ++ show port
      label = dcLabel (privDesc p \/ net) dcTrue
  guardAllocP p label
  let h = BlessedTCB label ioh
  hSetBufferingP p h IO.LineBuffering
  return (h, name, port)

--
-- The game
--

-- | Define a referee principal who can access both sockets.
referee :: DCPrivDesc
referee = toComponent $ principal "referee"

-- | These are the privileges of the referee.  Any code that accesses
-- this symbol is particularly security critical.
refereePriv :: Priv Component
refereePriv = MintTCB referee


data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
outcome _ _                  = Lose

getMove :: Handle -> DC Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..maxBound] :: [Move])
  input <- hGetLine h
  case reads input of
    (move, _):_ -> return move
    _           -> getMove h

play :: Handle -> LMVar DCLabel Move -> LMVar DCLabel Move -> DC ()
play h mvUs mvThem = do
  us <- getMove h
  putLMVar mvUs us
  them <- takeLMVarP refereePriv mvThem -- SECURITY CRITICAL
  let o = outcome us them
  hPutStrLn h $ "You " ++ show o

dcmain :: Socket -> DC ()
dcmain s = do
  (h1, _, _) <- acceptP refereePriv s
  hPutStrLnP refereePriv h1 "Waiting for another player..."
  (h2, _, _) <- acceptP refereePriv s
  mv1 <- newEmptyLMVar (labelOf h1)
  mv2 <- newEmptyLMVar (labelOf h2)
  forkLIO $ finallyP refereePriv (play h1 mv1 mv2) $ do
    tryPutLMVarP refereePriv mv1 $ error "The other player is dead"
    hCloseP refereePriv h1
  forkLIO $ finallyP refereePriv (play h2 mv2 mv1) $ do
    tryPutLMVarP refereePriv mv2 $ error "The other player is dead"
    hCloseP refereePriv h2
  dcmain s

main :: IO ()
main = IO.withSocketsDo $ do
  ios <- IO.listenOn port
  putStrLn $ "Listening on " ++ show port
  let s = BlessedTCB (dcLabel referee referee) ios
  evalDC $ dcmain s
