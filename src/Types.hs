{-# LANGUAGE InstanceSigs #-}
module Types where

import Constants
data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Enum, Eq)

instance Show Suit where
  show Spades = _SPADE_UNICODE_
  show Hearts = _HEART_UNICODE_
  show Diamonds = _DIAMOND_UNICODE_
  show Clubs = _CLUB_UNICODE_

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Enum, Eq, Ord)

data GameStage = PreFlop | Flop | Turn | River
  deriving (Enum, Eq, Show)

data WinningCondition = StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | OnePair | HighCard | Tie
  deriving (Eq, Show)

data HandComparison = HandComparison {
    winningHand :: [Card],
    winningCondition :: WinningCondition,
    handComparisonResult :: Ordering
  } deriving (Eq, Show)

instance Show Rank where
  show :: Rank -> String
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Card = Card {
        rank :: Rank,
        suit :: Suit,
        visible :: Bool
    } deriving (Eq)

-- Define Card an instance of Ord such that it can be sorted using its rank
instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare (Card r1 _ _) (Card r2 _ _) = compare r1 r2

instance Show Card where
    show (Card rank suit True) = show rank ++ show suit
    show (Card rank suit False) = "?"

type PlayerID = Int

data Player = Player {
    playerId :: PlayerID,
    playerName :: String,
    playerTokens :: Int,
    playerCards :: [Card],
    playerBet :: Int,
    playerFolded :: Bool,
    roundPlayed :: Bool
  } deriving (Eq)

instance Show Player where
    show p = playerName p ++ " (" ++ show (playerTokens p) ++ " tokens) " ++ show (playerCards p) ++ " bet: " ++ show (playerBet p) ++ " folded: " ++ show (playerFolded p) ++ "\n"

data Game = Game {
    community_cards :: [Card],
    players :: [Player],
    current_bet :: Int,
    pot :: Int,
    currentPlayerCycle :: Maybe [PlayerID],
    -- current_round :: Int,
    game_over :: Bool,
    gameStagesLeft :: [GameStage]
  }

instance Show Game where
  show g = "\nCommunity Cards: " ++ show (community_cards g) ++ "\n"
    ++ "Players: " ++ "\n"
    ++ concatMap show (players g)
    ++ "Current Bet: " ++ show (current_bet g) ++ "\n"
    ++ "Current Pool: " ++ show (pot g) ++ "\n"
    ++ "Game Over: " ++ show (game_over g) ++ "\n"
