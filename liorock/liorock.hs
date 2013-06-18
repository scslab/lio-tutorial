{-# LANGUAGE Safe #-}
module Main where

import Control.Monad

import LIO
import LIO.Concurrent
import LIO.DCLabel

import RockLib
import Play
import NetLib

port :: PortID
port = PortNumber 1617

runGame :: DCPriv -> Principal -> Handle -> LMVar DCLabel Move
           -> Handle -> LMVar DCLabel Move -> DC ()
runGame refereePriv pd us usmv them themmv = do
  game <- labelP refereePriv (True %% (privDesc refereePriv)) $
          GameState usmv themmv (show pd)
  let movefn = enterMoveP refereePriv
  setLabelP refereePriv (labelOf us)
  play us game movefn

main :: IO ()
main = withSocketsDo $ do
  evalDC $ do
    (sock, refereePriv) <- listenOn port
    logP refereePriv $ "Listening on " ++ show port
    setClearance (dcLabel (privDesc refereePriv) dcTrue)
    forever $ do
      (h1, p1) <- acceptP refereePriv sock
      hPutStrLnP refereePriv h1 "Waiting for another player..."
        `catch` \(SomeException e) -> return ()
      (h2, p2) <- acceptP refereePriv sock
      mv1 <- newEmptyLMVarP refereePriv (labelOf h1)
      mv2 <- newEmptyLMVarP refereePriv (labelOf h2)
      forkLIO $ runGame refereePriv p1 h1 mv1 h2 mv2
                  `finally` hCloseP refereePriv h1
      forkLIO $ runGame refereePriv p2 h2 mv2 h1 mv1
                  `finally` hCloseP refereePriv h2

