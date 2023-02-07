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
  deriving (Enum, Eq)

instance Show Rank where
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

instance Show Card where
    show (Card rank suit True) = show rank ++ show suit
    show (Card rank suit False) = "?"
data Player = Player {
    playerName :: String,
    playerTokens :: Int,
    playerCards :: [Card],
    playerBet :: Int,
    playerFolded :: Bool
  }

instance Show Player where
    show (Player name tokens cards bet folded) = name ++ " (" ++ show tokens ++ " tokens) " ++ show cards ++ " bet: " ++ show bet ++ " folded: " ++ show folded ++ "\n"

data Game = Game {
    community_cards :: [Card],
    players :: [Player],
    current_bet :: Int,
    pot :: Int,
    currentplayerCycle :: Maybe [Player],
    -- current_round :: Int,
    game_over :: Bool
  }

instance Show Game where
  show g = "\nCommunity Cards: " ++ show (community_cards g) ++ "\n"
    ++ "Current Player: " ++ show (current_player g) ++ "\n"
    ++ "Players: " ++ "\n"
    ++ concatMap show (players g)
    ++ "Current Bet: " ++ show (current_bet g) ++ "\n"
    ++ "Current Pool: " ++ show (pool g) ++ "\n"
    ++ "Game Over: " ++ show (game_over g) ++ "\n"
