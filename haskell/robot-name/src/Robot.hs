module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random
import Data.IORef

type Robot = IORef String
type RunState = [Name]
type Name = String

initialState :: RunState
initialState = []

mkRobot :: StateT RunState IO Robot
mkRobot = do
    names <- get
    name <- liftIO $ generateName names
    put (name : names)
    liftIO $ newIORef name

resetName :: Robot -> StateT RunState IO ()
resetName robot = do 
    names <- get
    name <- liftIO $ generateName names
    put (name : names)
    liftIO (writeIORef robot name)


robotName :: Robot -> IO String
robotName = readIORef

generateName :: RunState -> IO Name
generateName names = do
    name <- mapM randomRIO [letter, letter, digit, digit, digit]
    if name `elem` names then generateName names else return name
        where letter = ('A', 'Z')
              digit  = ('0', '9')
         