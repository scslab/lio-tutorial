
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import System.IO

port :: PortID
port = PortNumber 1617

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
outcome _ _                  = Lose

getMove :: Handle -> IO Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..maxBound] :: [Move])
  input <- hGetLine h
  case reads input of
    (move, _):_ -> return move
    _           -> getMove h

play :: Handle -> MVar Move -> MVar Move -> IO ()
play h mvUs mvThem = do
  us <- getMove h
  putMVar mvUs us
  them <- readMVar mvThem
  hPutStrLn h $ "You " ++ show (outcome us them)
    
main :: IO ()
main = withSocketsDo $ do
  s <- listenOn port
  putStrLn $ "Listening on " ++ show port
  forever $ do
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    
    (h1, n1, p1) <- accept s
    hSetBuffering h1 LineBuffering
    putStrLn $ "Player1: " ++ n1 ++ ":" ++ show p1
    _ <- forkIO $ play h1 mv1 mv2 `finally` do
      _ <- tryPutMVar mv1 $ error "The other player is dead"
      hClose h1

    (h2, n2, p2) <- accept s
    hSetBuffering h2 LineBuffering
    putStrLn $ "Player2: " ++ n2 ++ ":" ++ show p2
    forkIO $ play h2 mv2 mv1 `finally` do
      _ <- tryPutMVar mv2 $ error "The other player is dead"
      hClose h2

