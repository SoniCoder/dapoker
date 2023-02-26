module Test where
-- import isNothing
import Data.Maybe
import Main
import Types

hcTestTwoPair =
    let
        cc = [Card Eight Clubs True, Card King Spades True, Card Seven Hearts True, Card Six Spades True, Card Six Hearts True]
        h1 = [Card Ten Diamonds True, Card Ten Diamonds True]
        h2 = [Card Three Diamonds True, Card Nine Hearts True]
    in
    compareHandsTwoPair (h1 ++ cc) (h2 ++ cc)

hcTestThreeofAKind =
    let
        cc = [Card Eight Clubs True, Card King Spades True, Card Seven Hearts True, Card Ten Spades True, Card Six Hearts True]
        h1 = [Card Ten Diamonds True, Card Ten Clubs True]
        h2 = [Card Three Diamonds True, Card Nine Hearts True]
    in
    compareHandsThreeOfAKind (h1 ++ cc) (h2 ++ cc)

hcTestFullHouse =
    let
        cc = [Card Eight Clubs True, Card Six Spades True, Card Seven Hearts True, Card Ten Spades True, Card Six Hearts True]
        h1 = [Card Ten Diamonds True, Card Ten Clubs True]
        h2 = [Card Three Diamonds True, Card Nine Hearts True]
    in
    compareHandsFullHouse (h1 ++ cc) (h2 ++ cc)

hcTestFourceofAKind =
    let
        cc = [Card Eight Clubs True, Card King Spades True, Card Seven Hearts True, Card Ten Spades True, Card Ten Hearts True]
        h1 = [Card Ten Diamonds True, Card Ten Clubs True]
        h2 = [Card Three Diamonds True, Card Nine Hearts True]
    in
    compareHandsFourOfAKind (h1 ++ cc) (h2 ++ cc)

hcTestPair =
    let
        cc = [Card Eight Clubs True, Card King Spades True, Card Seven Hearts True, Card Five Spades True, Card Six Hearts True]
        h1 = [Card Ten Diamonds True, Card Ten Diamonds True]
        h2 = [Card Three Diamonds True, Card Nine Hearts True]
    in
    compareHandsPair (h1 ++ cc) (h2 ++ cc)

hcTest = do
    (h1, remDeck) <- drawCardsFromDeck 2 deck
    (h2, remDeck1) <- drawCardsFromDeck 2 remDeck
    (cc, _) <- drawCardsFromDeck 5 remDeck1
    putStr "Player 1 Hand: "
    print h1
    putStr "Player 2 Hand: "
    print h2
    return $ compareHands (h1 ++ cc) (h2 ++ cc)

testmaybe = do
    result <- Nothing
    result <- Just True >>= \x -> Just x
    return result

testcase = do
    inp <- getLine
    case inp of
        x | x == "a"  -> putStrLn "a"
        "" -> putStrLn "a"
        "a" -> putStrLn "a"
        "b" -> putStrLn "b"
        _ -> putStrLn "c"