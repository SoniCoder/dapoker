module Main where

import Actions
import Lib
import Types
import Constants
import System.Random
import Data.Maybe
-- import System.Random.Stateful (globalStdGen, uniformM)

deck :: [Card]
deck = [Card {rank = rank, suit = suit, visible = False} | rank <- [Two .. Ace], suit <- [Spades, Hearts, Diamonds, Clubs]]

drawCardsFromDeck :: Int -> [Card] -> IO ([Card], [Card])
drawCardsFromDeck n deck = do
  gen <- newStdGen
  let indices = take n $ randomRs (0, length deck - 1) gen
  let cards = map (deck !!) indices
  return (cards, filter (`notElem` cards) deck)

makeGame :: IO Game
makeGame = do
  putStrLn "Welcome to Haskell Poker!"
  putStr "Enter name of Player 1: "
  player1Name <- getLine
  (player1Cards, remDeck) <- drawCardsFromDeck 2 deck
  putStr "Enter name of Player 2: "
  let player1 = Player {
      playerName = player1Name,
      playerTokens = _PLAYER_INIT_TOKENS_,
      playerCards = player1Cards,
      playerBet = 0,
      playerFolded = False
    }
  player2Name <- getLine
  (player2Cards, remDeck1) <- drawCardsFromDeck 2 remDeck
  let player2 = Player {
      playerName = player2Name,
      playerTokens = _PLAYER_INIT_TOKENS_,
      playerCards = player2Cards,
      playerBet = 0,
      playerFolded = False
    }
  (communityCards, _) <- drawCardsFromDeck 5 remDeck1
  -- TODO: Use code like below to only set cards visible which belong to the current player
  -- let communityCards1 = map (\card -> card {visible = False}) communityCards
  return Game {
    community_cards = communityCards,
    current_player = Nothing,
    players = [player1, player2],
    current_bet = 0,
    pool = 0,
    game_over = False
  }

playGame :: IO Game -> IO ()
playGame g = do
  game <- g
  case current_player game of
    Nothing -> playGame g
    Just player -> playGame g

main :: IO ()
main = do
    putStrLn "Welcome to DaPoker!"
    putStrLn "This version of poker supports only 2 human players"
    putStrLn "The humans will play their turn one by one on the same machine"
    putStrLn "The humans will have to enter their names at the beginning"
    -- TODO: Get the initial constant player tokens from variable instead of hardcoding here
    putStrLn "Each player will be given 100 ADA tokens to start with"
    putStrLn "At the beginning of each round, a total of 5 community cards will be dealt"
    putStrLn "along with 2 cards to each player at random"
    putStrLn "The game will start with a small blind of 1 ADA and a big blind of 2 ADA"
    putStrLn "For simplicity the first player will be the small blind and the second player will be the big blind"
    putStrLn "This means that the game will start with the first player's turn"
    putStrLn "Each player has the option to fold, call, check, raise or perform allin"
    putStrLn "In this primitive version, there are no side pots, the winner takes all"
    playGame makeGame