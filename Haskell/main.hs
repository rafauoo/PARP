module Main where

import Map
import Data.List (find)
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
import Data.Maybe (isJust, fromJust, fromMaybe, maybe)

data GameState = GameState
  {currentLocation :: Location,
   playerWeaponLvl :: Int,
   monstersActual :: Monsters,
   currentQuest :: String,
   equipment :: [Key],
   worldMapActual :: WorldMap}

instructionsText :: [String]
instructionsText = [
    "Available commands are:",
    "",
    "instructions  -- to see these instructions.",
    "quit          -- to end the game and quit.",
    "w             -- go west.",
    "e             -- go east.",
    "n             -- go north.",
    "s             -- go south.",
    "bag           -- see your equipment",
    "quest         -- print current quest.",
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

death :: StateT GameState IO ()
death = do
    currentState <- get
    put currentState { currentLocation ="d4"}
    lift $ putStrLn $ ""
    lift $ putStrLn $ "Local medic has found your body..."
    lift $ putStrLn $ ""
    lift $ putStrLn $ ""
    look
look :: StateT GameState IO ()
look = do
    currentState <- get
    let loc = currentLocation currentState
    let weaponLvl = playerWeaponLvl currentState
    lift $ putStrLn $ "Your level: " ++ show weaponLvl
    case lookup loc descriptions of
        Just description -> lift $ putStrLn $ "You are in " ++ description
        Nothing -> lift $ putStrLn "Description not found for the current location."
    checkIfInExeter
    liftIO $ listCharactersAt loc characters
    let monsterInfo = checkMonster loc (monstersActual currentState)
    if isJust monsterInfo
        then do
            let (monsterName, monsterLevel) = fromMaybe ("Empty", 0) monsterInfo
            lift $ putStrLn $ "A wild " ++ monsterName ++ "[lvl " ++ show monsterLevel ++ "] appears!"
            monsterDefeated <- combat (monsterName, monsterLevel) weaponLvl
            if monsterDefeated
                then do
                    let addLvl = checkLvlUp loc lvlups
                    updateWeaponLvl (playerWeaponLvl currentState + addLvl)
                    removeMonster loc
                    addKeyIfInLocation keys
                    havekeys <- haveAllKeys keys
                    when (havekeys && (currentQuest currentState) == "Collect 3 key fragments.") $ do
                        let newConnection = ("a6", East, "a7")
                        addConnection newConnection
                        changeQuest "Talk to Jake in Anyor."
            else do
                death
                return ()
        else lift $ return ()

go :: Direction -> Location -> [Connection] -> Location
go dir loc map =
    case dir of
        North -> case find (\(l, d, l') -> l == loc && d == North) map of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc
        South -> case find (\(l, d, l') -> l == loc && d == South) map of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc
        East -> case find (\(l, d, l') -> l == loc && d == East) map of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc
        West -> case find (\(l, d, l') -> l == loc && d == West) map of
                    Just (_, _, newLoc) -> newLoc
                    Nothing -> loc

n :: StateT GameState IO String
n = do
    currentState <- get
    let worldMapActualValue = worldMapActual currentState
    let newLocation = go North (currentLocation currentState) worldMapActualValue
    put (currentState { currentLocation = newLocation })
    return newLocation

s :: StateT GameState IO String
s = do
    currentState <- get
    let worldMapActualValue = worldMapActual currentState
    let newLocation = go South (currentLocation currentState) worldMapActualValue
    put (currentState { currentLocation = newLocation })
    return newLocation

e :: StateT GameState IO String
e = do
    currentState <- get
    let worldMapActualValue = worldMapActual currentState
    let newLocation = go East (currentLocation currentState) worldMapActualValue
    put (currentState { currentLocation = newLocation })
    return newLocation

w :: StateT GameState IO String
w = do
    currentState <- get
    let worldMapActualValue = worldMapActual currentState
    let newLocation = go West (currentLocation currentState) worldMapActualValue
    put (currentState { currentLocation = newLocation })
    return newLocation

checkMonster :: Location -> [Monster] -> Maybe (String, Int)
checkMonster currentLocation monsters =
    case filter (\(monsterLocation, _, _) -> currentLocation == monsterLocation) monsters of
        [] -> Nothing
        [(_, name, lvl)] -> Just (name, lvl)

combat :: (String, Int) -> Int -> StateT GameState IO Bool
combat (name, monsterLvl) playerWeaponLvl = do
    lift $ putStrLn $ "You are battling " ++ name
    if playerWeaponLvl >= monsterLvl
        then do
            lift $ putStrLn $ "You have defeated " ++ name
            return True
        else do
            lift $ putStrLn $ "You were defeated by " ++ name
            return False

removeMonster :: String -> StateT GameState IO ()
removeMonster monsterLoc = do
    currentState <- get
    let currentMonsters = monstersActual currentState
        newMonsters = filter (\(loc, _, _) -> loc /= monsterLoc) currentMonsters
    put currentState { monstersActual = newMonsters }

updateWeaponLvl :: Int -> StateT GameState IO ()
updateWeaponLvl newLvl = do
    currentState <- get
    put currentState { playerWeaponLvl = newLvl }

updateLocation :: Location -> StateT GameState IO ()
updateLocation newLocation = do
    currentState <- get
    put currentState { currentLocation = newLocation }

checkLvlUp :: Location -> LvlUps -> Int
checkLvlUp currentLocation lvlups =
    case filter (\(lvlUpLoc, _) -> currentLocation == lvlUpLoc) lvlups of
        [] -> 0
        [(_, addLvl)] -> addLvl


haveAllKeys :: Keys -> StateT GameState IO Bool
haveAllKeys keys = do
    currentState <- get
    return $ length (equipment currentState) == length keys


addKeyIfInLocation :: Keys -> StateT GameState IO ()
addKeyIfInLocation keys = do
    currentState <- get
    let currentLoc = currentLocation currentState
        keyincurrLoc = filter (\(loc, _) -> loc == currentLoc) keys
    put currentState {equipment = equipment currentState ++ keyincurrLoc}


addConnection :: Connection -> StateT GameState IO ()
addConnection connection = do
    modify (\currentState -> currentState { worldMapActual = connection : worldMapActual currentState })


formatKey :: Key -> String
formatKey (_, keyName) = "- " ++ keyName

getFirstWord :: String -> String
getFirstWord str = case words str of
    (firstWord:_) -> firstWord
    _ -> ""

getSecondWord :: String -> String
getSecondWord str =
    case words str of
        (_:second:_) -> second
        _ -> ""

gameLoop :: StateT GameState IO ()
gameLoop = do
    look
    cmd <- lift readCommand
    currentState <- get
    case getFirstWord cmd of
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
        "bag" -> do
            lift $ putStrLn "\nMy equipment:"
            lift $ mapM_ (putStrLn . formatKey) (equipment currentState)
            lift $ putStrLn ""
            gameLoop
        "quest" -> do
            printCurrentQuest
            gameLoop
        "talk" -> do
            talk (getSecondWord cmd)
            gameLoop
        "quit" -> return ()
        _ -> do
            lift $ printLines ["Unknown command.", ""]
            gameLoop

changeQuest :: String -> StateT GameState IO ()
changeQuest newQuest = do
    currentState <- get
    lift $ putStrLn $ ""
    lift $ putStrLn $ "QUEST FINISHED: " ++ currentQuest currentState
    lift $ putStrLn $ ""
    put currentState { currentQuest = newQuest}
    printNewQuest

printNewQuest :: StateT GameState IO ()
printNewQuest = do
    currentState <- get
    lift $ putStrLn $ ""
    lift $ putStrLn $ "NEW QUEST ADDED: " ++ currentQuest currentState
    lift $ putStrLn $ ""

printCurrentQuest :: StateT GameState IO ()
printCurrentQuest = do
    currentState <- get
    lift $ putStrLn $ "CURRENT QUEST: " ++ currentQuest currentState

listCharactersAt :: Location -> Characters -> IO ()
listCharactersAt _ [] = return ()
listCharactersAt loc ((l, name):xs) =
    if loc == l
        then do
            putStrLn $ "Characters present: " ++ name
            return ()
        else listCharactersAt loc xs

talk :: String -> StateT GameState IO ()
talk name = do
    havekeys <- haveAllKeys keys
    if name == "Jake" && havekeys
        then do
            lift $ putStrLn $ "So it is true. You need to prepare. Find Exeter to get a better weapon."
            changeQuest "Find Exeter to get a better weapon."
            let newConnection = ("g5", West, "g4")
            addConnection newConnection
            return ()
        else do
            lift $ putStrLn $ "Cannot talk to him!"
            return ()

checkIfInExeter :: StateT GameState IO ()
checkIfInExeter = do
    currentState <- get
    let loc = currentLocation currentState
    if loc == "g4" && currentQuest currentState == "Find Exeter to get a better weapon." then do
        changeQuest "Kill Empheral Phantom."
        let addLvl = checkLvlUp loc lvlups
        updateWeaponLvl (playerWeaponLvl currentState + addLvl)
    else return ()

main :: IO ()
main = do
    let state = GameState {currentLocation="d4", playerWeaponLvl=1, monstersActual=monsters,
    currentQuest = "Collect 3 key fragments.", equipment = [], worldMapActual=worldMap}
    putStrLn "In a realm veiled by ancient lore, three guardians protected fragments of a mysterious key."
    putStrLn "Druid guarded the forest, the Undead Priest watched over the temple, and a formidable Goblin held the key fragment in treacherous caves."
    putStrLn "Legends whispered of an elusive entity, the Ephemeral Phantom, residing in the abandoned house, said to hold the power to shape the realm's destiny."
    putStrLn "Only a brave soul could confront this being and determine the realm's fate."

    putStrLn ""
    putStrLn $ "NEW QUEST ADDED: " ++ "Collect 3 key fragments."
    putStrLn ""
    printInstructions
    evalStateT gameLoop state