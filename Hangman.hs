--Deb Rezanka
--CS456
--Project 1

import System.IO
import System.Random
import Data.List.Split
import Data.Char
import Data.List

main = do 
    --get random word from words.txt
    --handle <- openFile "words.txt" ReadMode
    handle <- openFile "words.txt" ReadMode
    contents <- hGetContents handle
    let listOfWords = splitOn "|" contents
    let numWords = Prelude.length listOfWords
    num <- randomRIO (0, Prelude.length listOfWords)
    let word = listOfWords!!num
    let start = Prelude.replicate (Prelude.length word) '.'
    putStrLn $ "guessWord = " ++ word
    
    --print Logo
    printLogo

    putStrLn"Welcome to the game Hangman!\n"
    putStrLn"The objective in this game is to guess the word."
    putStrLn"You can enter both uppercase and lowercase letters."
    putStrLn"If you think you know the word, you can type it in."
    putStrLn"You will lose if you have guessed 10 letters wrong.\n"   
    putStrLn $ "This is the word you need to guess: " ++ start++"\n"
    
    --start game
    playGame word '\n' 0 1 start
 
playGame :: String -> Char ->  Int  -> Int ->  String -> IO()
playGame word guess numErr tries guessSoFar = do    
    if (guessSoFar == word || numErr >= 10)
       --Print out results
        then do
          putStrLn"---------------"
          putStrLn"--- Results ---"
          putStrLn"---------------\n"
          
          if (word == guessSoFar)
              then do
                  putStrLn "Congratulations you guessed the right word!"
              else do
                  putStrLn $ "You guessed the wrong word. The word was " ++ word ++ ".\n"
                  putStrLn "Better luck next time!\n"
        -- else continue game
        else do
            newGuess <- (getNewGuess guess tries)
            let indices = elemIndices newGuess word
            if indices == []
                then do
                    putStrLn""
                    putStrLn "That letter was incorrect.\n"                  
                    printOutput (numErr + 1) guessSoFar
                    g <- getChar
                    playGame word g (numErr + 1) (tries + 1) guessSoFar
                else do
                    putStrLn""
                    putStrLn "That letter was correct.\n"
                    let newGSF = updateGuessSF indices guessSoFar (newGuess:[]) [] 0
                    printOutput numErr newGSF
                    g <- getChar
                    playGame word g numErr (tries +1) newGSF                    
                                      
getNewGuess x y = do
    putStr $ (show y)++".     Enter the letter(s) you want to guess: "
    if x == '\n'
        then do            
            g <- getChar
            let guess = toLower g
            return guess
        else do 
            let guess = toLower x
            return guess
                                     
--use the old guessSoFar to create the new one                   
updateGuessSF :: [Int] -> String  -> [Char] -> [Char] -> Int -> [Char]                   
updateGuessSF indices [x] letter newGSF i = 
    if i `elem` indices then   
            newGSF++letter
        else 
            newGSF++(x:[]) 
updateGuessSF indices (x:xs) letter newGSF i = 
    if i `elem` indices then        
        updateGuessSF indices xs letter (newGSF++letter) (i + 1)
        else 
            updateGuessSF indices xs letter (newGSF++(x:[])) (i + 1)
 
printOutput :: Int ->String -> IO()         
printOutput errors guessSoFar = do
    putStrLn $ "The word including the letters you guessed: " ++ guessSoFar ++ "\n"
    if errors == 0
        then do printPic0
    else if errors == 1
        then do printPic1
    else if errors == 2
        then do  printPic2    
    else if errors == 3
        then do  printPic3
    else if errors == 4
        then do  printPic4
    else if errors == 5
        then do printPic5
    else if errors == 6
        then do printPic6
    else if errors == 7
        then do printPic7
    else if errors == 8
        then do printPic8
    else if errors == 9
        then do printPic9  
    else do printPic10         
                
printLogo = do    
    putStrLn"--------------------------------------------"
    putStrLn"| #  #   #   #   #  #### #   #   #   #   # |"
    putStrLn"| #  #  # #  ##  # #     ## ##  # #  ##  # |"
    putStrLn"| #### ##### # # # #  ## # # # ##### # # # |"
    putStrLn"| #  # #   # #  ## #   # #   # #   # #  ## |"
    putStrLn"| #  # #   # #   #  ###  #   # #   # #   # |"
    putStrLn"--------------------------------------------\n"
    
printPic0 = do
    putStrLn"Amount of wrong letters: 0\n"
    putStrLn"\n"
    putStrLn"\n"
    putStrLn"\n"
    putStrLn"____________\n"

printPic1 = do
    putStrLn"Amount of wrong letters: 1\n"
    putStrLn"\n"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"__|_________\n"

printPic2 = do
    putStrLn"Amount of wrong letters: 2\n"
    putStrLn"  _______"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"__|_________\n"

printPic3 = do
    putStrLn"Amount of wrong letters: 3\n"
    putStrLn"  _______"
    putStrLn"  |/"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"__|_________\n"

printPic4 = do
    putStrLn"Amount of wrong letters: 4\n"
    putStrLn"  _______"
    putStrLn"  |/   | "
    putStrLn"  |    O "
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"  |"
    putStrLn"__|_________\n"
   
printPic5 = do
    putStrLn"Amount of wrong letters: 5\n"
    putStrLn"  _______"
    putStrLn"  |/   | "
    putStrLn"  |    O "
    putStrLn"  |    |"
    putStrLn"  |    |"
    putStrLn"  |"
    putStrLn"__|_________\n"
 
printPic6 = do
    putStrLn"Amount of wrong letters: 6\n"
    putStrLn"  _______"
    putStrLn"  |/   | "
    putStrLn"  |    O "
    putStrLn"  |   \\|"
    putStrLn"  |    | "
    putStrLn"  |"
    putStrLn"__|_________\n"
   
printPic7 = do
    putStrLn"Amount of wrong letters: 7\n"
    putStrLn"  _______"
    putStrLn"  |/   | "
    putStrLn"  |    O "
    putStrLn"  |   \\|/"
    putStrLn"  |    | "
    putStrLn"  |"
    putStrLn"__|_________\n"
    
printPic8 = do
    putStrLn"Amount of wrong letters: 8\n"
    putStrLn"  _______"
    putStrLn"  |/   | "
    putStrLn"  |    O "
    putStrLn"  |   \\|/"
    putStrLn"  |    | "
    putStrLn"  |   /"
    putStrLn"__|_________\n"
   
printPic9 = do
    putStrLn"Amount of wrong letters: 9\n"
    putStrLn"  _______"
    putStrLn"  |/   | "
    putStrLn"  |    O "
    putStrLn"  |   \\|/"
    putStrLn"  |    | "
    putStrLn"  |   / \\"
    putStrLn"__|_________\n"
    
printPic10 = do
    putStrLn"Amount of wrong letters: 10\n"
    putStrLn"  _______"
    putStrLn"  |/   | "
    putStrLn"  |    X "
    putStrLn"  |   \\|/"
    putStrLn"  |    | "
    putStrLn"  |   / \\"
    putStrLn"__|_________\n"