module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot direction _) = direction

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coordinates) = coordinates

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction coordinates

move :: Robot -> String -> Robot
move robot []           = robot
move robot (x:xs) = move (step robot x) xs
        where step (Robot direction coordinates) c = case c of
                'L' -> Robot (turnLeft direction) coordinates
                'R' -> Robot (turnRight direction) coordinates
                'A' -> Robot direction (advance direction coordinates)

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance North (x, y) = (x, y + 1)
advance East (x, y) = (x + 1, y)
advance South (x, y) = (x, y - 1)
advance West (x, y) = (x - 1, y)