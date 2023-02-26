{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use null" #-}
module Main where

import Actions
import Lib
import Types
import Constants
import System.Random
import Data.Maybe
import Data.List
import Data.Ord
import Control.Applicative (Alternative, empty, (<|>))
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
  let indices = take n $ nub $ randomRs (0, length deck - 1) gen
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

compareHandsStraightFlush :: [Card] -> [Card] -> Maybe HandComparison
compareHandsStraightFlush h1 h2 =
  case hcresult h1 h2 of
    Just x -> Just $ HandComparison h1 StraightFlush x
    Nothing -> Nothing
  where
    hcresult h1 h2
          | isStraightFlush h1 && isStraightFlush h2 = Just $ compare (getHighCard h1) (getHighCard h2)
          | isStraightFlush h1 = Just GT
          | isStraightFlush h2 = Just LT
          | otherwise = Nothing

isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = areFourDup
  where
    ranks = map rank cards
    areTwoDup = length (nub ranks) <= 5
    singleDupRemoved = ranks \\ nub ranks
    doubleDupRemoved = singleDupRemoved \\ nub singleDupRemoved
    areFourDup = length (doubleDupRemoved \\ nub doubleDupRemoved) >= 1

getFourOfAKindRank :: [Card] -> Rank
getFourOfAKindRank cards = maximum (doubleDupRemoved \\ nub doubleDupRemoved)
  where
    ranks = map rank cards
    singleDupRemoved = ranks \\ nub ranks
    doubleDupRemoved = singleDupRemoved \\ nub singleDupRemoved

getBestFourOfAKindComb :: [Card] -> [Card]
getBestFourOfAKindComb cards =
  let fourOfAKindRank = getFourOfAKindRank cards
  in
  take 5 $ sortBy (flip (\c1 c2 ->
            case rank c1 == fourOfAKindRank of
              True -> GT
              False -> LT
              )) cards

compareHandsFourOfAKind :: [Card] -> [Card] -> Maybe HandComparison
compareHandsFourOfAKind h1 h2 =
  case hcresult h1 h2 of
      Just x -> Just $ HandComparison bestHand FourOfAKind x
      Nothing -> Nothing
    where
      hcresult h1 h2
          | isFourOfAKind h1 && isFourOfAKind h2 = Just $ compare (getFourOfAKindRank h1) (getFourOfAKindRank h2)
          | isFourOfAKind h1 = Just GT
          | isFourOfAKind h2 = Just LT
          | otherwise = Nothing
      bestHand = case hcresult h1 h2 of
            Just x -> case x of
              GT -> getBestFourOfAKindComb h1
              LT -> getBestFourOfAKindComb h2
              EQ -> getBestFourOfAKindComb h1
            Nothing -> []

isFullHouse :: [Card] -> Bool
isFullHouse cards =
  case isThreeOfAKind cards of
    True -> do 
      let tripletRemovedRanks = filter (/= getThreeOfAKindRank cards) ranks
      length (tripletRemovedRanks \\ nub tripletRemovedRanks) >= 1
    False -> False
  where
    ranks = map rank cards

getThreeOfAKindRank :: [Card] -> Rank
getThreeOfAKindRank cards = maximum (singleDupRemoved \\ nub singleDupRemoved)
  where
    ranks = map rank cards
    singleDupRemoved = ranks \\ nub ranks

compareHandsFullHouse :: [Card] -> [Card] -> Maybe HandComparison
compareHandsFullHouse h1 h2 =
  case hcresult h1 h2 of
    Just x -> Just $ HandComparison h1 FullHouse x
    Nothing -> Nothing
  where
    hcresult h1 h2
          | isFullHouse h1 && isFullHouse h2 = Just $ compare (getThreeOfAKindRank h1) (getThreeOfAKindRank h2)
          | isFullHouse h1 = Just GT
          | isFullHouse h2 = Just LT
          | otherwise = Nothing

compareHandsFlush :: [Card] -> [Card] -> Maybe HandComparison
compareHandsFlush h1 h2 =
  case hcresult h1 h2 of
    Just x -> Just $ HandComparison h1 Flush x
    Nothing -> Nothing
  where
    hcresult h1 h2
          | isFlush h1 && isFlush h2 = Just $ compare (getHighCard h1) (getHighCard h2)
          | isFlush h1 = Just GT
          | isFlush h2 = Just LT
          | otherwise = Nothing

compareHandsStraight :: [Card] -> [Card] -> Maybe HandComparison
compareHandsStraight h1 h2 =
  case hcresult h1 h2 of
    Just x -> Just $ HandComparison h1 Straight x
    Nothing -> Nothing
  where hcresult h1 h2
          | isStraight h1 && isStraight h2 = Just $ compare (getHighCard h1) (getHighCard h2)
          | isStraight h1 = Just GT
          | isStraight h2 = Just LT
          | otherwise = Nothing

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards = areThreeDup
  where
    ranks = map rank cards
    areTwoDup = length (nub ranks) <= 5
    singleDupRemoved = ranks \\ nub ranks
    areThreeDup = length (singleDupRemoved \\ nub singleDupRemoved) >= 1

getBestThreeOfAKindComb :: [Card] -> [Card]
getBestThreeOfAKindComb cards =
  let threeOfAKindRank = getThreeOfAKindRank cards
  in
  take 5 $ sortBy (flip (\c1 c2 ->
            case rank c1 == threeOfAKindRank of
              True -> GT
              False -> LT
              )) cards

compareHandsThreeOfAKind :: [Card] -> [Card] -> Maybe HandComparison
compareHandsThreeOfAKind h1 h2 =
  case hcresult h1 h2 of
    Just x -> Just $ HandComparison bestHand ThreeOfAKind x
    Nothing -> Nothing
  where
    hcresult h1 h2
          | isThreeOfAKind h1 && isThreeOfAKind h2 = Just $ compare (getThreeOfAKindRank h1) (getThreeOfAKindRank h2)
          | isThreeOfAKind h1 = Just GT
          | isThreeOfAKind h2 = Just LT
          | otherwise = Nothing
    bestHand = case hcresult h1 h2 of
            Just x -> case x of
              GT -> getBestThreeOfAKindComb h1
              LT -> getBestThreeOfAKindComb h2
              EQ -> getBestThreeOfAKindComb h1
            Nothing -> []
-- The list of Cards in input will have usually seven elements
isTwoPair :: [Card] -> Bool
isTwoPair cards = length (nub ranks) <= 5
  where
    ranks = map rank cards

-- In case of tie, higher rank pair ones then the second pair ones then the side card
-- To keep things simple let's just return the rank of the first pair
getTwoPairRank :: [Card] -> Rank
getTwoPairRank cards = maximum $ ranks \\ nub ranks
  where
    ranks = map rank cards

getBestTwoPairCombination :: [Card] -> [Card]
getBestTwoPairCombination cards =
  let twoPairRank = getTwoPairRank cards
  in
  take 5 $ sortBy (flip (\c1 c2 ->
            case rank c1 == twoPairRank of
              True -> GT
              False -> LT
              )) cards

compareHandsTwoPair :: [Card] -> [Card] -> Maybe HandComparison
compareHandsTwoPair h1 h2 =
  case hcresult h1 h2 of
    Just x -> Just $ HandComparison bestHand TwoPair x
    Nothing -> Nothing
  where
    hcresult h1 h2
      | isTwoPair h1 && isTwoPair h2 = Just $ compare (getTwoPairRank h1) (getTwoPairRank h2)
      | isTwoPair h1 = Just GT
      | isTwoPair h2 = Just LT
      | otherwise = Nothing
    bestHand = case hcresult h1 h2 of
      Just x -> case x of
        GT -> getBestTwoPairCombination h1
        LT -> getBestTwoPairCombination h2
        EQ -> getBestTwoPairCombination h1
      Nothing -> []

isPair :: [Card] -> Bool
isPair cards = length (nub ranks) <= 6
  where
    ranks = map rank cards

getPairRank :: [Card] -> Rank
getPairRank cards = maximum $ ranks \\ nub ranks
  where
    ranks = map rank cards

getBestPairCombination :: [Card] -> [Card]
getBestPairCombination cards =
  let pairRank = getPairRank cards
  in
  take 5 $ sortBy (flip (\c1 c2 ->
            case rank c1 == pairRank of
              True -> GT
              False -> LT
              )) cards

compareHandsPair :: [Card] -> [Card] -> Maybe HandComparison
compareHandsPair h1 h2 =
    case hcresult h1 h2 of
      Just x -> Just $ HandComparison {
        winningHand = bestHand,
        winningCondition = OnePair,
        handComparisonResult = x
      }
      Nothing -> Nothing
    where hcresult h1 h2
            | isPair h1 && isPair h2 = Just $ compare (getPairRank h1) (getPairRank h2)
            | isPair h1 = Just GT
            | isPair h2 = Just LT
            | otherwise = Nothing
          bestHand = case hcresult h1 h2 of
            Just x -> case x of
              GT -> getBestPairCombination h1
              LT -> getBestPairCombination h2
              EQ -> getBestPairCombination h1
            Nothing -> []

compareHighCard :: [Card] -> [Card] -> Maybe HandComparison
compareHighCard h1 h2 =
  let hcResult = compare (getHighCard h1) (getHighCard h2)
  in
    Just $ HandComparison {
      handComparisonResult = hcResult,
      winningCondition = HighCard,
      winningHand = bestHand
    } where bestHand = case hcResult of
                        GT -> bestCombinationh1
                        LT -> bestCombinationh2
                        EQ -> bestCombinationh1
                      where hcResult = compare (getHighCard h1) (getHighCard h2)
                            bestCombinationh1 = take 5 $ reverse $ sort h1
                            bestCombinationh2 = take 5 $ reverse $ sort h2
    -- TODO! correct the winningHand above

compareHands :: [Card] -> [Card] -> Maybe HandComparison
compareHands h1 h2 =
  compareHandsStraightFlush h1 h2 <|>
  compareHandsFourOfAKind h1 h2 <|>
  compareHandsFullHouse h1 h2 <|>
  compareHandsFlush h1 h2 <|>
  compareHandsStraight h1 h2 <|>
  compareHandsThreeOfAKind h1 h2 <|>
  compareHandsTwoPair h1 h2 <|>
  compareHandsPair h1 h2 <|>
  compareHighCard h1 h2

sortPlayerFunc :: [Card] -> Player -> Player -> Ordering
sortPlayerFunc comCards p1 p2 =
  maybe
  EQ handComparisonResult
  (compareHands
     (playerCards p1 ++ comCards) (playerCards p2 ++ comCards))

sortPlayersByWinningHand :: [Player] -> [Card] -> [Player]
sortPlayersByWinningHand players comCards =
  sortBy (sortPlayerFunc comCards) players


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
              let comCards = community_cards game
              let updatedGame = case gameStagesLeft game of
                    PreFlop : _ -> game {
                      community_cards = updateFirstN 3 (\card -> card {visible = True}) comCards,
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
                  let sortedPlayers = sortPlayersByWinningHand updatedPlayerList (community_cards updatedGame)
                  let winningPlayer = head sortedPlayers
                  putStr "Game Over! Winning Player: "
                  putStr $ show winningPlayer
                  let hcResult = compareHands (playerCards (head sortedPlayers) ++ comCards) (playerCards (sortedPlayers !! 1) ++ comCards)
                  let defaulthcResult = HandComparison {
                    winningHand = [],
                    winningCondition = Tie,
                    handComparisonResult = EQ
                  }
                  putStr "Winning Hand: "
                  print (winningHand $ fromMaybe defaulthcResult hcResult)
                  putStr "Winning Condition: "
                  print (winningCondition $ fromMaybe defaulthcResult hcResult)
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
                      putStr "Enter your action (default call): "
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
                        action | action == "call" || action == "" -> do
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