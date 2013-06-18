{-# LANGUAGE Trustworthy #-}

module NetLib
  ( Handle
  , hPutStrLn, hPutStrLnP, hGetLine
  , hSetBufferingP, hCloseP
  , listenOn, acceptP, PortID(..), IO.withSocketsDo
  , Socket
  ) where

import safe qualified Data.ByteString.Char8 as S8
import Network (PortID(..), HostName, PortNumber)
import qualified Network as IO
import safe qualified System.IO as IO

import safe LIO
import safe LIO.DCLabel
import LIO.TCB
import LIO.TCB.LObj

-- | simple LIO wrappers for system library functions.  Names that end
-- in P take an additional privilege argument.

type Handle = LObj DCLabel IO.Handle

hPutStrLnP :: PrivDesc l p =>
   Priv p -> LObj l IO.Handle -> String -> LIO l ()
hPutStrLnP p = blessPTCB IO.hPutStrLn p

hPutStrLn :: Label l => LObj l IO.Handle -> String -> LIO l ()
hPutStrLn h = blessTCB IO.hPutStrLn h

hGetLine :: Label l => LObj l IO.Handle -> LIO l String
hGetLine h = blessTCB IO.hGetLine h

hSetBufferingP :: PrivDesc l p =>
                  Priv p -> LObj l IO.Handle -> IO.BufferMode -> LIO l ()
hSetBufferingP p = blessPTCB IO.hSetBuffering p

hCloseP :: PrivDesc l p => Priv p -> LObj l IO.Handle -> LIO l ()
hCloseP p = blessPTCB IO.hClose p


type Socket = LObj DCLabel IO.Socket

listenOn :: PortID -> DC (Socket, DCPriv)
listenOn port = do
  sock <- blessTCB IO.listenOn (LObjTCB dcPub port)
  let net = principal $ "tcp://localhost:" ++ show port
      lbl = (net %% net)
      priv = PrivTCB $ toComponent net
  return (LObjTCB lbl sock, priv)

acceptP :: DCPriv -> Socket -> DC (Handle, Principal)
acceptP p s = do
  (ioh, name, port) <- blessPTCB IO.accept p s
  let net = principal $ "tcp://" ++ name ++ ":" ++ show port
      label = dcLabel (privDesc p \/ net) (privDesc p \/ net)
  guardAllocP p label
  let h = LObjTCB label ioh
  hSetBufferingP p h IO.LineBuffering
  return (h, net)

