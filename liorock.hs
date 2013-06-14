{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException(..), Exception(..))
import Control.Monad (liftM, void)
import qualified Data.ByteString.Char8 as S8
import Network (PortID(..), HostName, PortNumber)
import qualified Network as IO
import System.IO (BufferMode(..))
import qualified System.IO as IO
import qualified Data.List as L

import LIO hiding (tryLIO)
import LIO.Concurrent
import LIO.GuardIO.TCB
import LIO.DCLabel
import LIO.Privs.TCB
import LIO.TCB

port :: PortID
port = PortNumber 1617

--
-- Some DC functions for accessing IO abstractions
--

allocBlessTCB io p (LObjTCB l a) = guardIO lifter (io a)
  where lifter r = guardAllocP p l >> rethrowIoTCB r

taintBlessTCB io p (LObjTCB l a) = guardIO lifter (io a)
  where lifter r = taintP p l >> rethrowIoTCB r

--
--
--

-- | Labeled handle
type Handle = LObj DCLabel IO.Handle

-- | Write, no-read. 
-- Note: not correct since handle can be a handle to a socket, so this
-- may throw (implying read).
hPutStrLnP :: PrivDesc l p => Priv p -> LObj l IO.Handle -> String -> LIO l ()
hPutStrLnP p h str = do 
  l <- getLabel
  allocBlessTCB (\h' -> IO.hPutStrLn h' $ show l ++ ": " ++ str) p h

hPutStrLn :: Label l => LObj l IO.Handle -> String -> LIO l ()
hPutStrLn = hPutStrLnP noPrivs

-- | Read, no-write.
-- Note: not correct since it consume (and thus writes).
hGetLineP :: PrivDesc l p
          => Priv p -> LObj l IO.Handle -> LIO l (Labeled l String)
hGetLineP p h = do
  line <- taintBlessTCB IO.hGetLine p h
  labelP p (labelOf h) line

hGetLine :: Label l => LObj l IO.Handle -> LIO l (Labeled l String)
hGetLine = hGetLineP noPrivs

hSetBufferingP :: PrivDesc l p
               => Priv p -> LObj l IO.Handle -> BufferMode -> LIO l ()
hSetBufferingP p = blessTCB IO.hSetBuffering p

hCloseP :: PrivDesc l p => Priv p -> LObj l IO.Handle -> LIO l ()
hCloseP p = blessTCB IO.hClose p

type Socket = LObj DCLabel IO.Socket

acceptP :: DCPriv -> Socket -> DC (Handle, HostName, PortNumber)
acceptP p s = do
  (ioh, name, port) <- blessTCB IO.accept p s
  let net = principal $ S8.pack $ "net:" ++ name ++ ":" ++ show port
      sockLabel = dcLabel (privDesc p \/ net) anybody -- (privDesc p \/ net)
  guardAllocP p sockLabel
  let h = LObjTCB sockLabel ioh
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


data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
outcome _ _                  = Lose

-- Untrusted move? who cares
-- Can pass labeled input back to dcmain
--getMove :: DCPriv -> (Handle -> String -> DC()) -> Handle -> DC Move
getMove p h = do
  linput <- hGetLine h
  input  <- unlabel linput
  case reads input of
    (move, _):_ -> return move
    _           -> do hPutStrLn h "Try again:" 
                      getMove p h

dcmain :: Socket -> DC ()
dcmain s = do
  (h1, _, _) <- acceptP refereePriv s
  tryLIO_ $ hPutStrLn h1 "Waiting for another player..."
  (h2, _, _) <- acceptP refereePriv s

  let game = do r1 <- play h1
                r2 <- play h2
                v1 <- lWaitP refereePriv r1
                v2 <- lWaitP refereePriv r2
                hPutStrLn h1 $ "You "++ (show $ outcome v1 v2)
                hPutStrLn h2 $ "You "++ (show $ outcome v2 v1)

  let cleanup = do hCloseP refereePriv h1
                   hCloseP refereePriv h2

  tryLIOP_ refereePriv $ finallyP refereePriv game cleanup

  dcmain s

    where play h = do
            let l = labelOf h
                (Just hPriv) = dcDelegatePriv refereePriv $ dcSecrecy l
            lForkP refereePriv l $ do
              hPutStrLn h $ "Please enter one of "  ++
                              show ([minBound..maxBound] :: [Move])
              getMove hPriv h
          --
          refereePriv = MintTCB referee

main :: IO ()
main = IO.withSocketsDo $ do
  ios <- IO.listenOn port
  putStrLn $ "Listening on " ++ show port
  let s = LObjTCB (dcLabel referee referee) ios
  evalDC $ dcmain s

tryLIOP :: (Exception e, Label l, PrivDesc l p)
        => Priv p -> LIO l a -> LIO l (Either e a)
tryLIOP p act = catchLIOP p (Right `liftM` act) (return . Left)

tryLIO :: (Exception e, Label l)
       => LIO l a -> LIO l (Either e a)
tryLIO = tryLIOP noPrivs

tryLIOP_ :: (Label l, PrivDesc l p)
         => Priv p -> LIO l a -> LIO l (Either SomeException a)
tryLIOP_ = tryLIOP

tryLIO_ :: (Label l)
        => LIO l a -> LIO l (Either SomeException a)
tryLIO_ = tryLIO
