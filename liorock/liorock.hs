
module Main where

import Network (PortID (..), listenOn)
import qualified Network as IO

import LIO
import LIO.Concurrent
import LIO.DCLabel
import LIO.TCB
import LIO.TCB.LObj

import RockLib
import Play
import NetLib

-- | Define a referee principal who can access both sockets.
referee :: DCPrivDesc
referee = toComponent $ principal "referee"

refereePriv :: DCPriv
refereePriv = PrivTCB referee

port :: PortID
port = PortNumber 1617

runGame :: Principal -> Handle -> LMVar DCLabel Move
           -> Handle -> LMVar DCLabel Move -> DC ()
runGame pd us usmv them themmv = do
  game <- labelP refereePriv (True %% referee) $
          GameState usmv themmv (show pd)
  let movefn = enterMoveP refereePriv
  setLabelP refereePriv (labelOf us)
  play us game movefn

main :: IO ()
main = IO.withSocketsDo $ do
  ios <- IO.listenOn port
  putStrLn $ "Listening on " ++ show port
  let s = LObjTCB (referee %% referee) ios
      loop = do
        (h1, p1) <- acceptP refereePriv s
        hPutStrLnP refereePriv h1 "Waiting for another player..."
          `catch` \(SomeException e) -> return ()
        (h2, p2) <- acceptP refereePriv s
        mv1 <- newEmptyLMVarP refereePriv (labelOf h1)
        mv2 <- newEmptyLMVarP refereePriv (labelOf h2)
        forkLIO $ runGame p1 h1 mv1 h2 mv2 `finally` hCloseP refereePriv h1
        forkLIO $ runGame p2 h2 mv2 h1 mv1 `finally` hCloseP refereePriv h2
        loop
  evalDC $ setClearance (dcLabel referee dcTrue) >> loop
