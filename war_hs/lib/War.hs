module War (deal) where

import Data.List
main :: IO ()
main = do
    putStrLn "Hello, World!"

{--
Function stub(s) with type signatures for you to fill in are given below. 
Feel free to won as many additional helper functions as you want. 

The tests for these functions can be found in src/TestSuite.hs. 
You are encouraged to won your own tests in addition to those provided.

Run the tester by executing 'cabal test' from the war directory 
(the one containing war.cabal)
--}

-- The Deal Function takes the input, the shuffled deck of cards, converts all the 1's (Aces) to 14 since they have the highest value.
-- It plays the game with this updated deck, and converts all the 14s back to 1 before providing the pile of the winning player.

deal :: [Int] -> [Int]
deal shuf = let
    deck = reverse shuf
    convert = map (\x -> if x == 1 then 14 else x) deck
    converted_list = game convert
    in map (\x -> if x == 14 then 1 else x) converted_list

-- The cards are dealt to the players in an alternating fashion.
-- Player 1 gets the cards with an even index.
-- Player 2 gets the cards with an odd index.

p1_cards :: [a] -> [a]
p1_cards shuf = map fst $ filter (\(element, index) -> even index) $ zip shuf [0..52]

p2_cards :: [a] -> [a]
p2_cards shuf = map fst $ filter (\(element, index) -> odd index) $ zip shuf [0..52]

-- The Play function uses recursive calls to play the game. It takes in 3 inputs, player 1's cards, player 2's cards and pile to store tied cards.
-- When a player wins a round, the played cards are added to the bottom of the player's deck in descending order.

play :: [Int] -> [Int] -> [Int] -> [Int]
play [] p2_pile tie_pile = p2_pile ++ reverse (sort tie_pile) -- when player 1 runs out of cards, player 2 gets the cards and wins
play p1_pile [] tie_pile = p1_pile ++ reverse (sort tie_pile) -- when player 2 runs out of cards, player 1 gets the cards and wins
play p1_pile p2_pile tie_pile = let -- when both players have cards
    top1 = head p1_pile
    top2 = head p2_pile
    won = reverse $ sort (top1 : top2 : tie_pile) -- sorts the played cards and adds them to the tie pile in descending order
    -- Compare the played cards of player 1 and player 2
    in case compare top1 top2 of
        -- If player 1's top card is greater than player 2's top card, then player 1 takes the cards
        GT -> play (tail p1_pile ++ won) (tail p2_pile) []
        -- If player 1's top card is lesser than player 2's top card, then player 2 takes the cards
        LT -> play (tail p1_pile) (tail p2_pile ++ won) []
        -- If their top cards are equal, then war takes place
        EQ -> war p1_pile p2_pile won

-- The war function is used to determine the winner for a round where both players play the same card.
-- It checks if the players have more than 2 cards
-- During war, the players put one card facing down and play the next card. The card facing down gets added to the tie pile.
-- And this happens repeatedly until the cards played are not the same or if a player runs out of cards.
-- Finally when the players play cards that are not equal, the player playing the highest card takes all the cards.

war :: [Int] -> [Int] -> [Int] -> [Int]
war p1_pile p2_pile won = if length p1_pile >= 2 && length p2_pile >= 2
    then let
        (p1_first : p1_pile_rest) = tail p1_pile
        (p2_first : p2_pile_rest) = tail p2_pile
        in play p1_pile_rest p2_pile_rest (won ++ [p1_first, p2_first])
    else play (tail p1_pile) (tail p2_pile) won


-- The Game function takes the input deck of both players and plays the game until a winner is determined.

game :: [Int] -> [Int]
game shuf =
    let player_1 = p1_cards shuf
        player_2 = p2_cards shuf
    in play player_1 player_2 []          