{-# LANGUAGE DeriveDataTypeable #-}

module Main where      -- redundant since Main is the default
import Control.Concurrent
import Network -- (PortID(..))
import qualified Network as IO
import System.IO -- (BufferMode(..), IOMode(..))
import qualified System.IO as IO
import System.Environment

import Control.Concurrent
import Control.Exception
import Data.Typeable

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lose

{-
parseMove :: String -> Maybe Move
parseMove "Rock"     = Just Rock
parseMove "Paper"    = Just Paper
parseMove "Scissors" = Just Scissors
parseMove _          = Nothing
-}

{-
parseMove :: String -> Maybe Move
parseMove str = case reads str of [(m, "")] -> Just m
                                  _         -> Nothing
-}

parseMove :: String -> Maybe Move
parseMove str = case reads str of
  [(m, rest)] | ok rest -> Just m
  _                     -> Nothing
  where ok = all (`elem` " \r\n")

getMove :: IO.Handle -> IO Move
getMove h = do
  IO.hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  input <- IO.hGetLine h
  case parseMove input of Just move -> return move
                          Nothing -> getMove h

play :: Handle -> MVar Move -> String -> MVar Move -> IO ()
play h self otherName other = do
  myMove <- flip onException (putMVar self $ error "player dead") $ do
    hPutStrLn h $ "Playing against " ++ otherName
    getMove h
  putMVar self myMove
  otherMove <- readMVar other
  hPutStrLn h $ "You " ++ show (outcome myMove otherMove)

twoPlayerRock :: PortID -> IO ()
twoPlayerRock listenPort = bracket (IO.listenOn listenPort) sClose $ \s ->
  let loop = do
        (h1, host1, port1) <- IO.accept s
        hPutStrLn h1 "Waiting for your opponent to connect..."
            `catch` \(SomeException _) -> return ()
        let name1 = host1 ++ ":" ++ show port1
        (h2, host2, port2) <- IO.accept s
        let name2 = host2 ++ ":" ++ show port2
        mv1 <- newEmptyMVar
        mv2 <- newEmptyMVar
        forkIO $ play h1 mv1 name2 mv2 `finally` hClose h1
        forkIO $ play h2 mv2 name1 mv1 `finally` hClose h2
        loop
  in loop



computerVsUser :: Move -> IO.Handle -> IO ()
computerVsUser computerMove h = do
  userMove <- getMove h
  let o = outcome userMove computerMove
  IO.hPutStrLn h $ "You " ++ show o


greet :: IO.Handle -> IO ()
greet h = do
  IO.hPutStrLn h "What is your name?"
  fmap ("Hi, " ++) (IO.hGetLine h) >>= IO.hPutStrLn h
  -- name <- IO.hGetLine h
  -- IO.hPutStrLn h $ "Hi, " ++ name

withTty :: (IO.Handle -> IO a) -> IO a
withTty = IO.withFile "/dev/tty" ReadWriteMode

withClient :: PortID -> (IO.Handle -> IO a) -> IO a
withClient listenPort fn = do
  s <- IO.listenOn listenPort
  (h, host, port) <- IO.accept s
  putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
  IO.sClose s  -- Only accept one client
  a <- fn h
  IO.hClose h
  return a

withClients :: PortID -> (IO.Handle -> IO ()) -> IO ()
withClients listenPort fn = bracket (IO.listenOn listenPort) sClose $ \s ->
  let loop = do
        (h, host, port) <- IO.accept s
        putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
        forkIO $ fn h `finally` IO.hClose h
        loop
  in loop


ngreet :: IO ()
ngreet = IO.withSocketsDo $ do
  listenSock <- IO.listenOn (PortNumber 1617)
  (h, host, port) <- IO.accept listenSock
  IO.hSetBuffering h LineBuffering

  putStrLn $ "Connection from host " ++ host ++ " port " ++ show port

  IO.hPutStrLn h "What is your name?"
  name <- IO.hGetLine h
  IO.hPutStrLn h $ "Hi, " ++ name

  IO.hClose h
  IO.sClose listenSock


data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just action `catch` handler
    where handler (MyError msg) = do putStrLn msg; return Nothing

sayhi :: IO ()
sayhi = bracket (connectTo "localhost" (PortNumber 4123)) hClose $ \h -> do
  hPutStrLn h "hello"
