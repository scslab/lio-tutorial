{-# LANGUAGE Safe #-}

module RockLib where

import Control.Monad

import LIO
import LIO.DCLabel
import LIO.Concurrent

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
outcome _ _                  = Lose

data GameState = GameState {
    ourMove :: LMVar DCLabel Move
  , theirMove :: LMVar DCLabel Move
  , theirName :: String
  }

type Game = DCLabeled GameState

enterMoveP :: DCPriv -> Game -> Move -> DC Outcome
enterMoveP p lgs us = do
  unless (labelOf lgs `canFlowTo` (True %% p)) $
    fail "invalid label on game state"
  gs <- unlabel lgs
  ok <- tryPutLMVarP p (ourMove gs) us
  unless ok $ fail "already played"
  them <- readLMVarP p (theirMove gs)
  return $ outcome us them
