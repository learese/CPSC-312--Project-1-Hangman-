-- CPSC 312 - 2019 - Games in Haskell
module HangmanComp where

-- To run it, try:
-- ghci
-- :load Minimax

import Hangman
import Data.Map    (fromListWith, toList)
import Data.Char

--------   Determining the best move  ----------

compSmartGuess :: State -> Char
compSmartGuess (State (letters, g_letters, wbstate, word, guesses) available) =
  if (elem (fst (getGuess (State (letters, g_letters, wbstate, word, guesses) available))) available)
    then fst (getGuess (State (letters, g_letters, wbstate, word, guesses) available))
  else
    fst (makeLetterGuess (State (letters, g_letters, wbstate, word, guesses) available) letterRank1)

------------------ Letter Ranking -------------------------

letterRank1 :: [(Char, Int)]

letterRank1 = [('A', 5), ('B', 3), ('C', 2), ('D',  3), ('E', 5), ('F', 2), ('G', 2), ('H', 2), ('I', 4), ('J', 1), ('K', 2), ('L', 2), ('M',  2), ('N', 4), ('O', 3), ('P', 3), ('Q', 1), ('R', 5), ('S', 5), ('T' , 4), ('U' , 3), ('V', 1), ('W', 2), ('X', 1), ('Y', 3), ('Z',1) ]

--------- FILTER WORDBANK FOR POTENTIAL MATCHES -----------
-- removes words that don't match pattern

updateWordBankState :: State -> State
updateWordBankState (State (letters, g_letters, wbstate, word, guesses) available) = State (letters, g_letters, (filterWordBankLetter wbstate (getLetters letters)), word, guesses) available


-- filterWordBankLetter input the current State of the word, output all possible words that match current state
filterWordBankLetter :: [[Char]] -> [Char]-> [[Char]]
filterWordBankLetter [] _ = []
filterWordBankLetter wbstate letters = [word| word <- wbstate, (checkMatch letters word)]

-- checkMatch takes in the current state of the word and a word from the wordbank and returns true if the word is a possible match and false otherwise
checkMatch :: [Char] -> [Char] -> Bool
checkMatch [] _ = False
checkMatch _ [] = False
checkMatch (h:t) (x:y)
    | (t == [] && y == []) = if (h == x || h == '_') then True else False
    | (h == x || h == '_')  = checkMatch t y
    | otherwise = False

getLetters :: [Char] -> [Char]
getLetters [] = []
getLetters (h:t)
    | h == ' ' = getLetters t
    | otherwise = (toLower h): getLetters t


------------- ALGORITHM FOR DETERMINING COMPUTER'S GUESS --------------
-- returns the computer's guess

-- getGuess function which returns the Char guess of the computer
getGuess :: State -> (Char, Int)
getGuess (State (letters, g_letters, wbstate, word, guesses) available) =
  do
    let i = 0
    let listOfIndexes = getIndexes (getLetters letters) i
    let listOfGuesses = [getRank wbstate index available | index <- listOfIndexes]  -- get the list of possible letter guesses as a tuple with their rankings i.e ('s', 3)
    if (listOfGuesses == [])
      then (makeLetterGuess (State (letters, g_letters, wbstate, word, guesses) available) letterRank1) -- if no guesses, select a valid letter from list of ranked letters
    else
     getMaxRank ([getMaxRank guesses available| guesses <- listOfGuesses]) available

-- get the letter with the greatest occurrence in the wordbank at all index positions



-- try maxOccurrence (getRank (filterWordBank 4 wordbank1) 1)  -- output (s, 34)

-- makeLetterGuess
makeLetterGuess :: State -> [(Char, Int)] -> (Char, Int)
makeLetterGuess (State (letters, g_letters, wbstate, word, guesses) available) letterRank =
  do
    let count = 0
    let vowels = [('A', 5), ('E', 5), ('I', 4), ('O', 3), ('U', 3)]
    let updatedLetterRank = filterLetterRank available letterRank
    -- if (checkVowels (getLetters letters) count)
    -- then (getMaxRank [(vowel, rank)|(vowel, rank) <- vowels, (elem vowel available)] available) -- get a vowel as a guess that is still available
    -- else
    getMaxRank [(letter, rank) | (letter,  rank) <- updatedLetterRank, (elem letter available)] available

{-
-- checkVowels
checkVowels :: [Char] -> Int -> Bool
checkVowels [] count
      | count > 1 = False
      | otherwise = True
checkVowels (h:t) count =
    do
      let vowels = "aeiou"
      if (elem h vowels)
        then checkVowels t (count + 1)
        else checkVowels t count
-}

-- filterLetterRank input letterRank, output a list of tuples of letters that are still available_actions
filterLetterRank :: [Char] -> [(Char, Int)] -> [(Char, Int)]
filterLetterRank  [] _ = []
filterLetterRank available letterRank = [(letter, rank) | (letter, rank) <- letterRank, (elem letter available)]



-- getIndexes gets the indexes of all '_' in the current state of the word to be guessed
getIndexes :: [Char] -> Int -> [Int]
getIndexes [] _ = []
getIndexes (h:t) index
    | h == '_' = ((index+1): getIndexes t (index+1))
    | otherwise = getIndexes t (index+1)

-- getRank inputs a wordbank and an index to search at for each word in wordbank, at all the empty slots in the letters to be guessed, returns the letter with the most hits
getRank :: [[Char]] -> Int -> [Char] -> [(Char, Int)]
getRank wordbank index available =
  do
  let currIndexLetters = [ last (take index word) | word <- wordbank]  -- At a given index, return a list of all chars found for each word in wordbank at that given position index
  let currIndexGuessesRanked = occurrences currIndexLetters available -- for each letter found, if letter has not been guessed and available, then get the frequency of occurence and add to list of Guesses for the Index
  currIndexGuessesRanked

-- occurrences inputs a list of char letters and outputs a list of tuples (char, frequency) where char is the letter found and frequency is the number of occurences of that letter in letters
occurrences :: [Char] -> [Char] -> [(Char, Int)]
occurrences letters available = toList (fromListWith (+) [(letter, 1) | letter <- letters, (elem letter available)])

-- getMaxRank
getMaxRank :: [(Char, Int)] -> [Char] -> (Char, Int)
getMaxRank [] available = ((head available) , 0)
getMaxRank (h:t) _ = maxRest h t
  where maxRest (a,b) [] = (a,b)
        maxRest (a,b) (x:y)
          | ((b <= (snd x)) && (b > 0))  = maxRest x y
          | otherwise = maxRest (a,b) y


-------- Computer Hangman Player -----------------
simple_player :: Player
simple_player state = compSmartGuess state
