{-# LANGUAGE Safe #-}

module Play where

import LIO
import LIO.DCLabel

import RockLib
import NetLib

getMove :: Handle -> DC Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  input <- hGetLine h
  case reads input of
    (move, _):_ -> return move
    _           -> getMove h

play :: Handle
        -> Game
        -> (Game -> Move -> DC Outcome)
        -> DC ()
play h game movefn = do
  gs <- unlabel game
  hPutStrLn h $ "You are playing against " ++ theirName gs
  move <- getMove h
  o <- movefn game move
  hPutStrLn h $ "You " ++ show o
  
