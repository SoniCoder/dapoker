{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Main where

import Actions
import Lib
import Types
import Constants
import System.Random
import Data.Maybe
import Data.List
-- import System.Random.Stateful (globalStdGen, uniformM)

deck :: [Card]
-- deck = [Card {rank = rank, suit = suit, visible = False} | rank <- [Two .. Ace], suit <- [Spades, Hearts, Diamonds, Clubs]]
deck = [Card {rank = rank, suit = suit, visible = True} | rank <- [Two .. Ace], suit <- [Spades, Hearts, Diamonds, Clubs]]

randomElement :: [a] -> IO a
randomElement xs = do
  index <- randomRIO (0, length xs - 1)
  return (xs !! index)

updateFirstN :: Int -> (a -> a) -> [a] -> [a]
updateFirstN n f xs = go n xs
  where
    go _ [] = []
    go 0 xs = xs
    go k (x:xs) = f x : go (k-1) xs

-- pivots the cycle to start from a specific element
pivotCycle :: Eq a => [a] -> a -> [a]
pivotCycle [] _ = []
pivotCycle (x:xs) e
  | x == e = x:xs
  | otherwise = pivotCycle xs e

updateListAtIndex :: [a] -> Int -> a -> [a]  
updateListAtIndex list index newElement = take index list ++ [newElement] ++ drop (index + 1) list

drawCardsFromDeck :: Int -> [Card] -> IO ([Card], [Card])
drawCardsFromDeck n deck = do
  gen <- newStdGen
  let indices = take n $ randomRs (0, length deck - 1) gen
  let cards = map (deck !!) indices
  return (cards, filter (`notElem` cards) deck)

isFlush :: [Card] -> Bool
isFlush cards = length (nub suits) == 1
  where
    suits = map suit cards

isStraight :: [Card] -> Bool
isStraight cards = length (nub ranks) == 5 && (fromEnum (maximum ranks) - fromEnum (minimum ranks) == 4)
  where
    ranks = map rank cards

isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isStraight cards && isFlush cards

getHighCard :: [Card] -> Rank
getHighCard cards = maximum $ map rank cards

compareHandsStraightFlush :: [Card] -> [Card] -> Maybe Ordering
compareHandsStraightFlush h1 h2
  | isStraightFlush h1 && isStraightFlush h2 = Just $ compare (getHighCard h1) (getHighCard h2)
  | isStraightFlush h1 = Just GT
  | isStraightFlush h2 = Just LT
  | otherwise = Nothing

isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = length (nub ranks) == 2 && (length (filter (== head ranks) ranks) == 4 || length (filter (== head (tail ranks)) ranks) == 4)
  where
    ranks = map rank cards

getFourOfAKindRank :: [Card] -> Rank
getFourOfAKindRank cards
  | length (filter (== head ranks) ranks) == 4 = head ranks
  | otherwise = head (tail ranks)
  where
    ranks = map rank cards

compareHandsFourOfAKind :: [Card] -> [Card] -> Maybe Ordering
compareHandsFourOfAKind h1 h2
  | isFourOfAKind h1 && isFourOfAKind h2 = Just $ compare (getFourOfAKindRank h1) (getFourOfAKindRank h2)
  | isFourOfAKind h1 = Just GT
  | isFourOfAKind h2 = Just LT
  | otherwise = Nothing

isFullHouse :: [Card] -> Bool
isFullHouse cards = length (nub ranks) == 2 && (length (filter (== head ranks) ranks) == 3 || length (filter (== head (tail ranks)) ranks) == 3)
  where
    ranks = map rank cards

getThreeOfAKindRank :: [Card] -> Rank
getThreeOfAKindRank cards
  | length (filter (== head ranks) ranks) == 3 = head ranks
  | otherwise = head (tail ranks)
  where
    ranks = map rank cards

compareHandsFullHouse :: [Card] -> [Card] -> Maybe Ordering
compareHandsFullHouse h1 h2
  | isFullHouse h1 && isFullHouse h2 = Just $ compare (getThreeOfAKindRank h1) (getThreeOfAKindRank h2)
  | isFullHouse h1 = Just GT
  | isFullHouse h2 = Just LT
  | otherwise = Nothing

compareHandsFlush :: [Card] -> [Card] -> Maybe Ordering
compareHandsFlush h1 h2
  | isFlush h1 && isFlush h2 = Just $ compare (getHighCard h1) (getHighCard h2)
  | isFlush h1 = Just GT
  | isFlush h2 = Just LT
  | otherwise = Nothing

compareHandsStraight :: [Card] -> [Card] -> Maybe Ordering
compareHandsStraight h1 h2
  | isStraight h1 && isStraight h2 = Just $ compare (getHighCard h1) (getHighCard h2)
  | isStraight h1 = Just GT
  | isStraight h2 = Just LT
  | otherwise = Nothing

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards = length (nub ranks) == 3 && (length (filter (== head ranks) ranks) == 3 || length (filter (== head (tail ranks)) ranks) == 3 || length (filter (== head (tail (tail ranks))) ranks) == 3)
  where
    ranks = map rank cards

compareHandsThreeOfAKind :: [Card] -> [Card] -> Maybe Ordering
compareHandsThreeOfAKind h1 h2
  | isThreeOfAKind h1 && isThreeOfAKind h2 = Just $ compare (getThreeOfAKindRank h1) (getThreeOfAKindRank h2)
  | isThreeOfAKind h1 = Just GT
  | isThreeOfAKind h2 = Just LT
  | otherwise = Nothing

isTwoPair :: [Card] -> Bool
isTwoPair cards = length (nub ranks) == 3 && (length (filter (== head ranks) ranks) == 2 || length (filter (== head (tail ranks)) ranks) == 2 || length (filter (== head (tail (tail ranks))) ranks) == 2)
  where
    ranks = map rank cards

getTwoPairRank :: [Card] -> Rank
getTwoPairRank cards
  | length (filter (== head ranks) ranks) == 2 = head ranks
  | length (filter (== head (tail ranks)) ranks) == 2 = head (tail ranks)
  | otherwise = head (tail (tail ranks))
  where
    ranks = map rank cards

compareHandsTwoPair :: [Card] -> [Card] -> Maybe Ordering
compareHandsTwoPair h1 h2
  | isTwoPair h1 && isTwoPair h2 = Just $ compare (getTwoPairRank h1) (getTwoPairRank h2)
  | isTwoPair h1 = Just GT
  | isTwoPair h2 = Just LT
  | otherwise = Nothing

isPair :: [Card] -> Bool
isPair cards = length (nub ranks) == 4 && (length (filter (== head ranks) ranks) == 2 || length (filter (== head (tail ranks)) ranks) == 2 || length (filter (== head (tail (tail ranks))) ranks) == 2 || length (filter (== head (tail (tail (tail ranks)))) ranks) == 2)
  where
    ranks = map rank cards

getPairRank :: [Card] -> Rank
getPairRank cards
  | length (filter (== head ranks) ranks) == 2 = head ranks
  | length (filter (== head (tail ranks)) ranks) == 2 = head (tail ranks)
  | length (filter (== head (tail (tail ranks))) ranks) == 2 = head (tail (tail ranks))
  | otherwise = head (tail (tail (tail ranks)))
  where
    ranks = map rank cards

compareHandsPair :: [Card] -> [Card] -> Maybe Ordering
compareHandsPair h1 h2
  | isPair h1 && isPair h2 = Just $ compare (getPairRank h1) (getPairRank h2)
  | isPair h1 = Just GT
  | isPair h2 = Just LT
  | otherwise = Nothing

compareHands :: [Card] -> [Card] -> Ordering
compareHands h1 h2 =
  case compareHandsStraightFlush h1 h2 of
    Just x -> x
    Nothing ->
      case compareHandsFourOfAKind h1 h2 of
        Just x -> x
        Nothing ->
          case compareHandsFullHouse h1 h2 of
            Just x -> x
            Nothing ->
              case compareHandsFlush h1 h2 of
                Just x -> x
                Nothing ->
                  case compareHandsStraight h1 h2 of
                    Just x -> x
                    Nothing ->
                      case compareHandsThreeOfAKind h1 h2 of
                        Just x -> x
                        Nothing ->
                          case compareHandsTwoPair h1 h2 of
                            Just x -> x
                            Nothing ->
                              case compareHandsPair h1 h2 of
                                Just x -> x
                                Nothing -> compare (getHighCard h1) (getHighCard h2)
      
  

sortPlayersByWinningHand :: [Player] -> [Card] -> [Player]
sortPlayersByWinningHand players comCards =
  sortBy (\p1 p2 -> compareHands (playerCards p1 ++ comCards) (playerCards p2 ++ comCards)) players


makeGame :: IO Game
makeGame = do
  putStr "Enter name of Player 1: "
  player1Name <- getLine
  (player1Cards, remDeck) <- drawCardsFromDeck 2 deck
  putStr "Enter name of Player 2: "
  let player1 = Player {
      playerId = 0,
      playerName = player1Name,
      playerTokens = _PLAYER_INIT_TOKENS_,
      playerCards = player1Cards,
      playerBet = 0,
      playerFolded = False,
      roundPlayed = False
    }
  player2Name <- getLine
  (player2Cards, remDeck1) <- drawCardsFromDeck 2 remDeck
  let player2 = Player {
      playerId = 1,
      playerName = player2Name,
      playerTokens = _PLAYER_INIT_TOKENS_,
      playerCards = player2Cards,
      playerBet = 0,
      playerFolded = False,
      roundPlayed = False
    }
  (communityCards, _) <- drawCardsFromDeck 5 remDeck1
  -- TODO: Use code like below to only set cards visible which belong to the current player
  let communityCards1 = map (\card -> card {visible = False}) communityCards
  return Game {
    community_cards = communityCards1,
    players = [player1, player2],
    currentPlayerCycle = Nothing,
    current_bet = 0,
    pot = 0,
    game_over = False,
    gameStagesLeft = [PreFlop, Flop, Turn, River]
  }

playGame :: IO Game -> IO ()
playGame g = do
  game <- g
  case currentPlayerCycle game of
    Nothing -> do
      -- The starting player sequence isn't ready
      -- Initialize the currentPlayerCycle by selecting a random player
      -- and cycling the players list
      sbPlayer <- randomElement (players game)
      let updatedSbPlayer = sbPlayer {
        playerBet = 1,
        playerTokens = playerTokens sbPlayer - 1
      }
      let updatedPlayerList0 = updateListAtIndex (players game) (playerId sbPlayer) updatedSbPlayer
      let playerIdCycle = pivotCycle (cycle $ map playerId $ players game) (playerId sbPlayer)
      let bbPlayer = players game !! (playerIdCycle !! 1)
      let updatedBbPlayer = bbPlayer {
        playerBet = 2,
        playerTokens = playerTokens bbPlayer - 2
      }
      let updatedPlayerList = updateListAtIndex updatedPlayerList0 (playerId bbPlayer) updatedBbPlayer
      -- set randPlayer as the small blind and the next player as big blind
      -- then start the game from the player after the big blind
      playGame $ return game {
        pot = 1 + 2,
        current_bet = 2,
        players = updatedPlayerList,
        currentPlayerCycle = Just playerIdCycle,
        gameStagesLeft = gameStagesLeft game
      }
    Just playerCycle -> do
      -- the player cycle is ready
      -- before proceeding with the current player, make sure that the player has not folded
      -- and has not played this round with current bet, otherwise just move to the next stage of the game
      -- folded means just move to the next player
      -- let curPlayer = hd playerCycle
      let currentPlayerId = head playerCycle
      let currentPlayer = players game !! currentPlayerId
      case playerFolded currentPlayer of
        True -> do
          -- the current player has folded
          -- move to the next player
          playGame $ return game {
            currentPlayerCycle = Just (tail playerCycle)
          }
        False -> do
          -- the current player has not folded
          -- check if the player has played this round
          case roundPlayed currentPlayer of
            True -> do
              -- the current player has played this round
              -- need to make sure that his current bet is equal to the current bet
              -- if not, then he needs to match the current bet
              -- if he has enough tokens to match the current bet, then he can play
              -- otherwise, he will be considered folded
              -- For now let's assume the player has already matched the current bet
              -- Since the player has matched the current bet, we can move to the next stage of the game
              -- For that set roundPlayed to False for all players
              -- Along with that, reveal community cards as required
              let updatedPlayerList = map (\player -> player {roundPlayed = False, playerBet = 0}) (players game)
              let updatedGame = case gameStagesLeft game of
                    PreFlop : _ -> game {
                      community_cards = updateFirstN 3 (\card -> card {visible = True}) (community_cards game),
                      gameStagesLeft = tail $ gameStagesLeft game
                    }
                    Flop : _ -> game {
                      community_cards = updateFirstN 4 (\card -> card {visible = True}) (community_cards game),
                      gameStagesLeft = tail $ gameStagesLeft game
                    }
                    Turn : _ -> game {
                      community_cards = updateFirstN 5 (\card -> card {visible = True}) (community_cards game),
                      gameStagesLeft = tail $ gameStagesLeft game
                    }
                    River: _ -> game {
                        game_over = True
                      }
                    [] -> game {
                        game_over = True
                      }
              case game_over updatedGame of
                True -> do
                  -- the game is over
                  -- TODO: print the winner(s) and distribute the pot
                  let winningPlayer = head $ sortPlayersByWinningHand updatedPlayerList (community_cards updatedGame)
                  putStrLn "Game Over! Winning Player:"
                  print winningPlayer
                  putStrLn "Enter any character to continue with the next game"
                  _ <- getChar
                  playGame makeGame
                False -> do
                  -- the game is not over
                  -- move to the next player
                  playGame $ return updatedGame {
                    players = updatedPlayerList,
                    currentPlayerCycle = Just playerCycle,
                    current_bet = 0
                  }
            False -> do
              -- the current player has not played this round
              -- check if the player has enough tokens to play
              case playerTokens currentPlayer of
                0 -> do
                  -- the current player has no tokens left
                  -- move to the next player
                  playGame $ return game {
                    currentPlayerCycle = Just (tail playerCycle)
                  }
                _ -> do
                  -- the current player has enough tokens to play
                  -- check if the player has enough tokens to match the current bet
                  -- case playerTokens currentPlayer >= current_bet game of
                  --   True -> do
                      -- the current player has enough tokens to match the current bet
                      -- ask the player to perform an action
                      putStrLn $ "Current Game Stage: " ++ show (head $ gameStagesLeft game)
                      putStrLn $ "Current player: " ++ playerName currentPlayer
                      putStrLn $ "Current bet: " ++ show (current_bet game)
                      putStrLn $ "Current pot: " ++ show (pot game)
                      putStrLn $ "Current player tokens: " ++ show (playerTokens currentPlayer)
                      putStrLn $ "Current player bet: " ++ show (playerBet currentPlayer)
                      putStrLn $ "Current player cards: " ++ show (playerCards currentPlayer)
                      putStrLn $ "Community cards: " ++ show (community_cards game)
                      putStr "Enter your action: "
                      action <- getLine
                      case action of
                        "fold" -> do
                          -- the player has folded
                          -- update the player's fold status
                          -- move to the next player
                          let updatedPlayer = currentPlayer {
                            playerFolded = True
                          }
                          let updatedPlayerList = updateListAtIndex (players game) currentPlayerId updatedPlayer
                          playGame $ return game {
                            players = updatedPlayerList,
                            currentPlayerCycle = Just (tail playerCycle)
                          }
                        "call" -> do
                          -- the player has called
                          -- update the player's bet and tokens
                          -- update the pot
                          -- move to the next player
                          let extraTokensNeeded = current_bet game - playerBet currentPlayer
                          let updatedPlayer = currentPlayer {
                            playerBet = current_bet game,
                            playerTokens = playerTokens currentPlayer - extraTokensNeeded,
                            roundPlayed = True
                          }
                          let updatedPlayerList = updateListAtIndex (players game) currentPlayerId updatedPlayer
                          playGame $ return game {
                            pot = pot game + extraTokensNeeded,
                            players = updatedPlayerList,
                            currentPlayerCycle = Just (tail playerCycle)
                          }
                        "raise" -> do
                          -- the player has raised
                          -- ask the player to enter the raise amount
                          putStrLn "Enter the raise amount: "
                          raiseAmount <- getLine
                          let raiseAmountInt = read raiseAmount :: Int
                          -- update the player's bet and tokens
                          -- update the pot
                          -- update the current bet
                          -- move to the next player
                          let updatedPlayer = currentPlayer {
                            playerBet = playerBet currentPlayer + raiseAmountInt + current_bet game,
                            playerTokens = playerTokens currentPlayer - raiseAmountInt - current_bet game,
                            roundPlayed = True
                          }
                          let updatedPlayerList = updateListAtIndex (players game) currentPlayerId updatedPlayer
                          playGame $ return game {
                            pot = pot game + raiseAmountInt + current_bet game,
                            current_bet = current_bet game + raiseAmountInt,
                            players = updatedPlayerList,
                            currentPlayerCycle = Just (tail playerCycle)
                          }
                        _ -> do
                          -- the player has entered an invalid action
                          -- ask the player to enter a valid action
                          putStrLn "Invalid action"
                          playGame g


      playGame g
  -- case current_player game of
  --   Nothing -> playGame g
  --   Just player -> playGame g

main :: IO ()
main = do
  -- Create an ASCII art logo saying - "DaPoker!" and print it
  logo <- readFile "logo.txt"
  putStrLn logo
  -- Read the rules from rules.txt and print them
  rules <- readFile "rules.txt"
  putStrLn rules
  playGame makeGame