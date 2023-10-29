module Main where

import Map
import Data.List (find)
import Control.Monad.State

data GameState = GameState { currentLocation :: Location }

instructionsText :: [String]
instructionsText = [
    "Available commands are:",
    "",
    "instructions  -- to see these instructions.",
    "quit          -- to end the game and quit.",
    ""
    ]

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printInstructions :: IO ()
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    getLine

go :: Direction -> Location -> WorldMap -> Location
go dir loc worldMap =
    case dir of
        North -> case find (\(l, d, l') -> l == loc && d == North) worldMap of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc
        South -> case find (\(l, d, l') -> l == loc && d == South) worldMap of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc
        East -> case find (\(l, d, l') -> l == loc && d == East) worldMap of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc
        West -> case find (\(l, d, l') -> l == loc && d == West) worldMap of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc

n :: StateT GameState IO String
n = do
    currentState <- get
    let newLocation = go North (currentLocation currentState) worldMap
    put (currentState { currentLocation = newLocation })
    return newLocation

s :: StateT GameState IO String
s = do
    currentState <- get
    let newLocation = go South (currentLocation currentState) worldMap
    put (currentState { currentLocation = newLocation })
    return newLocation

e :: StateT GameState IO String
e = do
    currentState <- get
    let newLocation = go East (currentLocation currentState) worldMap
    put (currentState { currentLocation = newLocation })
    return newLocation

w :: StateT GameState IO String
w = do
    currentState <- get
    let newLocation = go West (currentLocation currentState) worldMap
    put (currentState { currentLocation = newLocation })
    return newLocation

gameLoop :: StateT GameState IO ()
gameLoop = do
    cmd <- lift readCommand
    currentState <- get
    case cmd of
        "instructions" -> do
            lift printInstructions
            gameLoop
        "n" -> do
            newLoc <- n
            if currentLocation currentState == newLoc then
                lift $ putStrLn "You can't go that way!"
                else
                    lift $ putStrLn $ "You are now at location " ++ newLoc
            gameLoop
        "w" -> do
            newLoc <- w
            if currentLocation currentState == newLoc then
                lift $ putStrLn "You can't go that way!"
                else
                    lift $ putStrLn $ "You are now at location " ++ newLoc
            gameLoop
        "s" -> do
            newLoc <- s
            if currentLocation currentState == newLoc then
                lift $ putStrLn "You can't go that way!"
                else
                    lift $ putStrLn $ "You are now at location " ++ newLoc
            gameLoop
        "e" -> do
            newLoc <- e
            if currentLocation currentState == newLoc then
                lift $ putStrLn "You can't go that way!"
                else
                    lift $ putStrLn $ "You are now at location " ++ newLoc
            gameLoop
        "quit" -> return ()
        _ -> do
            lift $ printLines ["Unknown command.", ""]
            gameLoop

main :: IO ()
main = do
    let state = GameState {currentLocation="d4"}
    printInstructions
    evalStateT gameLoop state
