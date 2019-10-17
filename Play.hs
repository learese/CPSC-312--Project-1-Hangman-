-- CPSC 312 - 2019 - Project 1
-- Brendan David Chung - 10580744
-- Leann Santa Cruz - 24928525

module Play where

-- To run it, try:
-- ghci
-- :load Play

import Hangman
import HangmanComp
import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)

type TournammentState = (Int,Int)   -- wins, losses

play :: Game -> State -> Player -> TournammentState -> IO TournammentState
-- the Player begins a new game.
play game start_state opponent ts =
  let (wins, losses) = ts in
  do
      putStrLn "\n"
      putStrLn "This project was developed by Brendan Chung and Leann Santa Cruz based on Hangman."
      putStrLn "Hangman is a paper and pencil guessing game for two or more players. One player thinks of a word,"
      putStrLn "phrase or sentence and the other(s) tries to guess it by suggesting letters or numbers,"
      putStrLn "within a certain number of guesses. (from Wikipedia)"
      putStrLn "\n"
      putStrLn ("Tournament Results: "++show wins++ " wins "++show losses++" losses")
      putStrLn "Press 1 to start guessing."
      putStrLn "Press 2 to set a word for the computer to guess."
      putStrLn "Otherwise, press anything else to exit."
      line1 <- getLine
      if line1 == "1"
        then
          do
            putStrLn "\n"
            putStrLn "Please type a random number."
            line2 <- getLine
            putStrLn "\n"
            let idx = (read (removeChar line2) :: Int)
            person_play game (ContinueGame (setStartState (setWord(idx)))) opponent ts
      else if line1 == "2"
        then
          do
            putStrLn "\n"
            putStrLn "Enter a word for the computer to guess."
            line3 <- getLine
            putStrLn "\n"
            computer_play game (ContinueGame (setStartState (remove line3))) opponent ts
      else
          do
            putStrLn "\n"
            putStrLn "Thanks for playing! Here's the final score. (Wins, Losses)"
            return ts



person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- the Player guesses a letter in the word. Main body.
person_play game (EndOfGame val start_state) opponent ts =
  do
    newts <- update_tournament_state (val) ts
    play game start_state opponent newts

person_play game (ContinueGame state) opponent ts =
   do
      let State internal avail = state
      putStrLn(unlines(fullImage(6 - (displayGuessCount state))))
      putStrLn ("You have " ++show ((displayGuessCount state) + 1)++ " guess(es) left.")
      putStrLn ("Your word to guess has "++show(getWordLength(state))++" letter(s).")
      putStrLn (show state)
      line <- getLine
      let action = (readMaybe line :: Maybe Char)
      if (action == Nothing) || not ((fromJust action) `elem` avail)
        then  -- error; redo
          do
           putStrLn "Please type a letter from a to z that you haven't used already. Make sure you add quotation marks around it. e.g. 'a' or 'z'."
           putStrLn "\n"
           person_play game (ContinueGame state) opponent ts
        else if (checkGuess (fromJust action) state)
          then
            do
              putStrLn "You guessed correctly!"
              putStrLn "\n"
              person_play game (game (fromJust action) state) opponent ts
        else
            do
              putStrLn "You guessed incorrectly."
              putStrLn "\n"
              person_play game (game (fromJust action) state) opponent ts



computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent ts
-- person entered a word, the computer now guesses that word
computer_play game (EndOfGame val  start_state) opponent ts =
   do
      newts <- update_tournament_state (-val) ts
      play game start_state opponent newts

computer_play game (ContinueGame state) opponent ts =
      let
          newState = updateWordBankState state
          opponent_move = opponent newState
        in
          do
            putStrLn "\n"
            putStrLn (show newState)
            putStrLn "\n"
            putStrLn(unlines(fullImage(6 - (displayGuessCount newState))))
            putStrLn ("Number of guess(es) left: " ++show ((displayGuessCount newState) + 1)++ ".")
            putStrLn ("The word to guess has "++show(getWordLength(newState))++" letter(s).")
            putStrLn ("The computer chose "++show opponent_move)
            if (checkGuess opponent_move newState)
              then
                do
                  putStrLn "The computer guessed correctly!"
                  computer_play game (game opponent_move newState) opponent ts
            else
              do
                putStrLn "The computer guessed incorrectly."
                computer_play game (game opponent_move newState) opponent ts



update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value a value to the Player, the Tournament State, return the new Tournament State
update_tournament_state val (wins, losses)
  | val == 1 = do
      putStrLn "You guessed the word correctly!"
      return (wins + 1, losses)
  | val == -1 = do
      putStrLn "The computer guessed correctly! You lost."
      return (wins, losses + 1)
  | val == 2 = do
      putStrLn "You did not guess the word correctly."
      return (wins, losses + 1)
  | otherwise = do
      putStrLn "The computer guessed incorrectly. You won!"
      return (wins + 1, losses)
