module Main where

import System.IO.Unsafe  -- be careful!                                          
import System.Random 
import Debug.Trace
import Data.Matrix
import Data.Ratio

data Element = 
    Dirt (Int, Int) | 
    Obstacle (Int, Int) |
    Crib (Int, Int) |
    Child (Int, Int) Bool |
    Robot (Int, Int) Bool 
    deriving (Show, Eq)



main :: IO ()
-- main =  print (replaceElement  (Dirt (0,1)) (Dirt (0,3)) [Dirt (0,1),Dirt (0,-1),Dirt (-1,0) ,Child (0,2) True])
-- main =  print (moveChildren 5 5 [Obstacle (2,1), Obstacle (4,1),Obstacle (3,0), Obstacle (3,2), Obstacle (3,3),Child (3,1) False] [Obstacle (2,1), Obstacle (4,1), Obstacle (3,0), Obstacle (3,2), Obstacle (3,3), Child (3,1) False]) 
-- main =  print (getFreeCells3x3 5 5 (2, 2) [Obstacle (2,1), Obstacle (4,1),Obstacle (3,0), Obstacle (3,2), Obstacle (3,3),Child (3,1) False]) 
-- main =  print (generateDirt 5 5 [Child (3, 1) False, Child (2,3) False] [Obstacle (2,1), Obstacle (4,1),Obstacle (3,0), Obstacle (3,2), Obstacle (3,3),Child (3,1) False, Child (2,4) False] 1) 
-- main =  print (moveAgents 5 5 [Robot (0,4) True, Obstacle (4,1),Obstacle (3,0), Dirt (0,4), Obstacle (3,3),Child (3,1) False, Child (0,4) True] [Robot (0,4) True, Obstacle (4,1),Obstacle (3,0), Dirt (0,4), Obstacle (3,3),Child (3,1) False, Child (0,4) True]) 
-- main =  print (moveTowardsElement "Dirt" (2,5) 5 5 [Robot (2,5) False, Dirt (1, 5)] 1)
main =  print (simulation 11 11 5 1 20 4 200 2)


simulation :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Element]
simulation n m children robots dirt obstacles maxT t= 
    let initEnv = initialize n m children robots dirt obstacles
    in traceShow (boardPrint initEnv (newSMatrix n m)) (mainLoop n m children t 0 maxT initEnv)

mainLoop :: Int -> Int -> Int -> Int -> Int -> Int -> [Element] -> [Element]
mainLoop n m childrenCount t time maxT env = 
    let agents = getRobots env env
        env0 = moveAgents n m agents env
        env1 = moveChildren n m env0 env0
        dirtCount = getDirtCount childrenCount
        children = getChildren env0 env1
        env2 = if mod time t == 0 
                    then traceShow ["GENERATING DIRT----TIME:", show time] (generateDirt n m children env1 dirtCount)
                         else env1
        in if isGameOver n m env2 maxT time
            then traceShow (boardPrint env2 (newSMatrix n m)) env2 
                else traceShow (boardPrint env2 (newSMatrix n m)) (mainLoop n m childrenCount t (time + 1) maxT env2)

isGameOver :: Int -> Int -> [Element] -> Int -> Int  ->  Bool
isGameOver n m env maxT time = let dirt = fromIntegral (getDirtyCellsCount env)
                                   total = fromIntegral (n * m)
                                   dirtPercentage = round ((dirt / total) * 100)
                                   freeBabiesCount = length (getChildren env env)
                                   in if time >= maxT 
                                          then traceShow ["END OF SIMULATION: TIME EXCEEDED ---- DIRT PERCENTAGE:" ++ show dirtPercentage] True
                                             else dirtPercentage > 40 &&  traceShow ["END OF SIMULATION: TOO MUCH DIRT----TIME:" ++ show time ++ "----DIRT PERCENTAGE:" ++ show dirtPercentage] True 

getDirtyCellsCount :: [Element] -> Int
getDirtyCellsCount [] = 0
getDirtyCellsCount (Dirt _: rest) =  1 + getDirtyCellsCount rest
getDirtyCellsCount (_ : rest) = getDirtyCellsCount rest

--Initialization
initialize :: Int -> Int -> Int -> Int -> Int -> Int -> [Element]
initialize n m children robots dirt obstacles = 
    let env1 = putCrib n m children
        env2 = putChildren n m children env1
        env3 = putRobots n m robots env2
        env4 = putObstacles n m obstacles env3
    in putDirt n m dirt env4

putCrib :: Int -> Int -> Int -> [Element]
putCrib n m children = 
    let x = randomInt 1 n
        y = randomInt 1 m 
    in putCribCell n m (children - 1) (x, y) [Crib (x, y)]

putCribCell :: Int -> Int -> Int -> (Int, Int) -> [Element]-> [Element]
putCribCell _ _ 0 (_, _) env = env
putCribCell n m cellCount (x1, y1) env = 
    let direction = randomInt 0 3
    in case direction of
        0 -> if isValidMove n m (x1, y1 - 1) env then let newEnv = env ++ [Crib (x1, y1 - 1)] in putCribCell n m (cellCount - 1) (x1, y1 - 1) newEnv else putCribCell n m cellCount (x1, y1) env 
        1 -> if isValidMove n m (x1, y1 + 1) env then let newEnv = env ++ [Crib (x1, y1 + 1)] in putCribCell n m (cellCount - 1) (x1, y1 + 1) newEnv else putCribCell n m cellCount (x1, y1) env 
        2 -> if isValidMove n m (x1 - 1, y1) env then let newEnv = env ++ [Crib (x1 - 1, y1)] in putCribCell n m (cellCount - 1) (x1 - 1, y1) newEnv else putCribCell n m cellCount (x1, y1) env 
        3 -> if isValidMove n m (x1 + 1, y1) env then let newEnv = env ++ [Crib (x1 + 1, y1)] in putCribCell n m (cellCount - 1) (x1 + 1, y1) newEnv else putCribCell n m cellCount (x1, y1) env 

putChildren :: Int -> Int -> Int -> [Element] -> [Element]
putChildren _ _ 0 env = env
putChildren n m childCount env =
    let x = randomInt 1 n 
        y = randomInt 1 m 
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Child (x, y) False] in putChildren n m (childCount - 1) newEnv
            else putChildren n m childCount env

putRobots :: Int -> Int -> Int -> [Element] -> [Element]
putRobots _ _ 0 env = env
putRobots n m robotCount env =
    let x = randomInt 1 n
        y = randomInt 1 m
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Robot (x, y) False] in putRobots n m (robotCount - 1) newEnv
            else putRobots n m robotCount env

putObstacles :: Int -> Int -> Int -> [Element] -> [Element]
putObstacles _ _ 0 env = env
putObstacles n m obsCount env =
    let x = randomInt 1 n
        y = randomInt 1 m
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Obstacle (x, y)] in putObstacles n m (obsCount - 1) newEnv
            else putObstacles n m obsCount env

putDirt :: Int -> Int -> Int -> [Element] -> [Element]
putDirt _ _ 0 env = env
putDirt n m dirtCount env =
    let x = randomInt 1 n
        y = randomInt 1 m
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Dirt (x, y)] in putDirt n m (dirtCount - 1) newEnv
            else putDirt n m dirtCount env

-------------------

--Agents
moveAgents :: Int -> Int -> [Element] -> [Element] -> [Element]
moveAgents _ _ [] env = env
moveAgents n m (Robot (x, y) c: rest) env = let newEnv = moveAgent n m env (Robot (x, y) c) in moveAgents n m rest newEnv
moveAgents n m (_ : rest) env = moveAgents n m rest env

moveAgent :: Int -> Int -> [Element] -> Element -> [Element]
moveAgent n m env (Robot (x, y) c) = 
    let elements' = getElementAt env (x, y)
        elements = removeElement (Robot (x, y) c) elements'
        freeChildren = getChildren env env
        childrenLength = length freeChildren 
    in if onDirt elements then removeElement (Dirt (x, y)) env
        else case elements of
            [] -> if childrenLength == 0 then moveTowardsDirt (Robot (x, y) c) n m env else moveTowardsChildren (Robot (x, y) c) n m env
            [Crib _, Child _ _] | c ->
                                        let newEnv = replaceElement (Child (x, y) c) (Child (x, y) False) env
                                            in replaceElement (Robot (x, y) c) (Robot (x, y) False) newEnv
                                        | childrenLength == 0 -> moveTowardsDirt (Robot (x, y) c) n m env
                                        | otherwise -> moveTowardsChildren (Robot (x, y) c) n m env
            [Child _ _, Crib _] | c ->
                                        let newEnv = replaceElement (Child (x, y) c) (Child (x, y) False) env
                                            in replaceElement (Robot (x, y) c) (Robot (x, y) False) newEnv
                                        | childrenLength == 0 -> moveTowardsDirt (Robot (x, y) c) n m env
                                        | otherwise -> moveTowardsChildren (Robot (x, y) c) n m env

            [Child _ _] -> if childrenLength == 0 then moveTowardsDirt (Robot (x, y) c) n m env else moveTowardsCrib  (Robot (x, y) c) n m env
            
            _ -> if childrenLength == 0 then moveTowardsDirt (Robot (x, y) c) n m env else moveTowardsChildren (Robot (x, y) c) n m env


moveTowardsDirt :: Element -> Int -> Int -> [Element] -> [Element]
moveTowardsDirt (Robot (x, y) c) n m env  =
     let nextPos = moveTowardsElement "Dirt" (x, y) n m env 1 c
         nextPosCell = getElementAt env nextPos 
        in if nextPos == (-1, -1) 
            then env else 
                if length nextPosCell == 1 && isChild (head nextPosCell) && not c
                    then let env1 = replaceElement (Robot (x, y) c) (Robot nextPos True) env
                            in replaceElement (Child nextPos False) (Child nextPos True) env1
                        else let env2 = replaceElement (Robot (x, y) c) (Robot nextPos c) env
                                in if c then replaceElement (Child (x, y) c) (Child nextPos c) env2 else env2

moveTowardsChildren :: Element -> Int -> Int -> [Element] -> [Element]
moveTowardsChildren (Robot (x, y) c) n m env  =
     let nextPos = moveTowardsElement "Child" (x, y) n m env 1 c
         nextPosCell = getElementAt env nextPos 
         in if nextPos == (-1, -1) 
            then env else 
                if length nextPosCell == 1 && isChild (head nextPosCell) && not c
                    then let env1 = replaceElement (Robot (x, y) c) (Robot nextPos True) env
                            in replaceElement (Child nextPos False) (Child nextPos True) env1
                        else let env2 = replaceElement (Robot (x, y) c) (Robot nextPos c) env
                                in if c then replaceElement (Child (x, y) c) (Child nextPos c) env2 else env2

moveTowardsCrib :: Element -> Int -> Int -> [Element] -> [Element]
moveTowardsCrib (Robot (x, y) c) n m env  =
     let nextPos = moveTowardsElement "Crib" (x, y) n m env 1 c
         nextPosCell = getElementAt env nextPos 
         in if nextPos == (-1, -1) 
            then env else 
                if length nextPosCell == 1 && isChild (head nextPosCell) && not c
                    then let env1 = replaceElement (Robot (x, y) c) (Robot nextPos True) env
                            in replaceElement (Child nextPos False) (Child nextPos True) env1
                        else let env2 = replaceElement (Robot (x, y) c) (Robot nextPos c) env
                                in if c then replaceElement (Child (x, y) c) (Child nextPos c) env2 else env2
---------------

--bfs
moveTowardsElement :: String -> (Int, Int) -> Int -> Int -> [Element] -> Int -> Bool -> (Int, Int)
moveTowardsElement destName (x, y) n m env dist c= 
    let matrix = newMatrix n m
        matrix1 = setElem 0 (x, y) matrix
        ((dx, dy), matrix2) = bfs [(x,y)] destName matrix1 env c
        in if (dx, dy) == (-1, -1) then (-1, -1) else getNextCell (dx, dy) matrix2 dist c
        -- in traceShow ((dx, dy), matrix2) (if (dx, dy) == (-1, -1) then (-1, -1) else getNextCell (dx, dy) matrix2 dist)

getNextCell :: (Int, Int) -> Matrix Int -> Int -> Bool ->  (Int, Int)
getNextCell (x, y) matrix dest c = 
     if matrix ! (x, y) == 1 || (matrix ! (x, y) == 2 && c)
         then (x, y) 
         else let newPos = goBack (x, y) matrix in getNextCell newPos matrix dest c
        --  else traceShow "BEFORE GOBACK"( let newPos = goBack (x, y) matrix in getNextCell newPos matrix dest)

goBack :: (Int, Int) -> Matrix Int -> (Int, Int)
goBack (x, y) matrix = 
    let current = matrix ! (x, y)
        dest = current - 1
        a =  (x + 1, y)
        b =  (x - 1, y)
        c =  (x, y + 1)
        d =  (x, y - 1)
        in if isInBoard (x + 1) y (nrows matrix) (ncols matrix) && matrix ! a == dest then a else
        -- in traceShow dest( if isInBoard (x + 1) y (nrows matrix) (ncols matrix) && matrix ! a == dest then a else
           if isInBoard (x - 1) y (nrows matrix) (ncols matrix) && matrix ! b == dest then b else
           if isInBoard x (y + 1) (nrows matrix) (ncols matrix) && matrix ! c == dest then c else d
        --    if isInBoard x (y + 1) (nrows matrix) (ncols matrix) && matrix ! c == dest then c else traceShow d d


bfs :: [(Int, Int)] -> String -> Matrix Int -> [Element] -> Bool -> ((Int, Int), Matrix Int)
bfs [] _ matrix _ _ = ((-1, -1), matrix)
-- bfs [] _ matrix _ _ = traceShow "NOT ACCESSIBLE" ((-1, -1), matrix)
bfs ((x, y): elements ) destName matrix env c= 
    let dist = matrix ! (x, y)
        (newElements, matrix1) = getAdjacents (x, y) elements env matrix dist c
        cell = getElementAt env (x, y) 
        -- in traceShow ["ADJACENTS",show newElements](if isElementIn cell destName && (matrix1 ! (x, y) /= 0)
        in if isElementIn cell destName && (matrix1 ! (x, y) /= 0)
            then ((x, y), matrix1) 
            else bfs newElements destName matrix1 env c
            -- else traceShow matrix1 (bfs newElements destName matrix1 env c)

getAdjacents :: (Int, Int) -> [(Int, Int)] -> [Element] -> Matrix Int -> Int -> Bool -> ([(Int, Int)], Matrix Int)
getAdjacents (x, y) elements env matrix dist c= 
    let adjacents =  [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]
        in getValidAdjacents elements adjacents env matrix dist c
 

getValidAdjacents :: [(Int, Int)] -> [(Int, Int)] -> [Element] -> Matrix Int -> Int -> Bool -> ([(Int, Int)], Matrix Int)
getValidAdjacents elements [] _ matrix _ _= (elements, matrix)
getValidAdjacents elements ((a, b) : adjacents) env matrix dist carried = 

    let cell =  getElementAt env (a, b)
        -- in traceShow ["VALID",show (a, b), show cell]( if isInBoard a b (nrows matrix) (ncols matrix) &&
        in  if isInBoard a b (nrows matrix) (ncols matrix) &&
              matrix ! (a, b) == -1 &&
              notElem (a,b) elements &&
              not (findObstacle cell) &&
              length cell <= 1 &&
              not (isElementIn cell "Robot") &&
              (carried && not (isElementIn cell "Child") || not carried)
            then 
                let newMatrix = setElem (dist + 1) (a,b) matrix 
                            in getValidAdjacents (elements ++ [(a, b)]) adjacents env newMatrix dist carried
            else getValidAdjacents elements adjacents env matrix dist carried
-- Move Children
moveChildren :: Int -> Int -> [Element] -> [Element] -> [Element]
moveChildren n m currentEnv [] = currentEnv
moveChildren n m currentEnv (Child (x, y) isCarried : elements)  = 
    let direction = randomInt 0 4
        child = Child (x, y) isCarried
    in case direction of
        0 -> moveChildren n m currentEnv elements --Doesn't move
        2 -> if isInBoard x (y-1) n m && not isCarried && not (isInCrib (x, y) currentEnv) then (let newEnv = moveChild n m currentEnv child (0, -1) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Left
        1 -> if isInBoard x (y+1) n m && not isCarried && not (isInCrib (x, y) currentEnv) then (let newEnv = moveChild n m currentEnv child (0,  1) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Right
        4 -> if isInBoard (x-1) y n m && not isCarried && not (isInCrib (x, y) currentEnv) then (let newEnv = moveChild n m currentEnv child (-1, 0) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Top
        3 -> if isInBoard (x+1) y n m && not isCarried && not (isInCrib (x, y) currentEnv) then (let newEnv = moveChild n m currentEnv child (1,  0) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Bottom

moveChildren n m currentEnv (el : rest) =  moveChildren n m currentEnv rest
 
moveChild :: Int -> Int -> [Element] -> Element -> (Int, Int) -> [Element]
moveChild n m env (Child (x, y) isCarried) (dx, dy) = 
    let newPos = (x + dx, y + dy)
        child = Child (x, y) isCarried
        newPosChild = Child newPos isCarried
        newPosEl = getElementAt env newPos
    in case newPosEl of
        [] -> replaceElement child newPosChild env
        _ -> let newEnv = pushObstacles n m newPosEl env (dx, dy) 
                in if newEnv == env 
                    then env 
                    else moveChild n m newEnv (Child (x, y) isCarried) (dx, dy) 

moveChild n m env _ (xd, yd) = env

pushObstacles :: Int -> Int -> [Element] -> [Element] -> (Int, Int) -> [Element]
pushObstacles n m [Obstacle (x, y)] env (xd, yd) = 
    if isInBoard (x + xd) (y + yd) n m then 
    let newPos = (x + xd, y + yd)
        newPosEl = getElementAt env newPos
        in if isOccupied newPos env
        then let newEnv = pushObstacles n m newPosEl env (xd, yd)
                in if newEnv == env 
                    then env 
                    else pushObstacles n m [Obstacle (x, y)] newEnv (xd, yd)
        else replaceElement (Obstacle (x, y)) (Obstacle newPos) env
    else env
pushObstacles _ _ _ env (_, _) = env

-------------------

--Generate Dirt
generateDirt :: Int -> Int -> [Element] -> [Element] -> Int -> [Element]
generateDirt n m [] env dirtCount = env
generateDirt n m _ env 0 = env
generateDirt n m (Child (x, y) _ : children) env dirtCount = 
    let freeCells = getFreeCells3x3 n m (x, y) env
        freeCellsCount = length freeCells
        currentDirtCount = randomInt 0 dirtCount
        newEnv = generateDirtInGrid freeCells currentDirtCount env
    in generateDirt n m children newEnv (dirtCount - currentDirtCount)
    -- in traceShow (show ("LOOP STATE:", children, env, dirtCount)) (generateDirt n m children newEnv (dirtCount - currentDirtCount))


generateDirtInGrid :: [(Int,Int)] -> Int -> [Element] -> [Element]
generateDirtInGrid _  0 env = env
generateDirtInGrid [] _ env = env
generateDirtInGrid freeCells dirtCount env =
    let freeCellsLength = length freeCells
        index = randomInt 0 (freeCellsLength - 1)
        newPos = freeCells!!index
        newEnv = env ++ [Dirt newPos]
        updateCells = removeElement newPos freeCells
        in generateDirtInGrid updateCells (dirtCount - 1) newEnv
-------------------

--Utils
newMatrix :: Int -> Int -> Matrix Int
newMatrix n m = matrix n m $ \(i, j) -> (-1)

newSMatrix :: Int -> Int -> Matrix String
newSMatrix n m = matrix n m $ \(i, j) -> "---"

randomInt :: Int -> Int -> Int 
{-# NOINLINE randomInt #-}
randomInt min max = unsafePerformIO (getStdRandom (randomR (min, max)))

isChild :: Element -> Bool
isChild (Child (a, b) c) = True 
isChild _ = False

row :: Element -> Int
row (Dirt (x, y)) = x
row (Obstacle (x, y)) = x
row (Crib (x, y)) = x
row (Child (x, y) c) = x
row (Robot (x, y) c) = x

getColumn :: Element -> Int
getColumn (Dirt (x, y)) = y
getColumn (Obstacle (x, y)) = y
getColumn (Crib (x, y)) = y
getColumn (Child (x, y) c) = y
getColumn (Robot (x, y) c) = y

isOccupied :: (Int, Int) -> [Element] -> Bool
isOccupied (x, y) [] = False
isOccupied (x, y) (el : rest) = 
     let elx = row el
         ely = getColumn el
         in elx == x && ely == y
         || isOccupied (x, y) rest

isInBoard :: Int -> Int -> Int -> Int-> Bool
isInBoard x y n m = 1 <= x && x <= n && 1 <= y && y <= m

isValidMove :: Int -> Int -> (Int, Int) -> [Element] -> Bool
isValidMove n m (x, y) env = isInBoard x y n m && not (isOccupied (x, y) env) 

removeElement :: Eq t => t -> [t] -> [t]
removeElement _ [] = []
removeElement element (e1 : rest) 
    | element == e1 = removeElement element rest 
    | otherwise = e1 : removeElement element rest

replaceElement :: Element -> Element -> [Element] -> [Element]
replaceElement oldE newE env = newE : removeElement oldE env 

getElementAt :: [Element] -> (Int, Int) -> [Element]
getElementAt env (r, c) = do
  x <- env
  if ( let re = row x
           ce = getColumn x
        in re == r && ce == c
     )
    then y ++ [x]
    else y
    where y = []

getFreeCells3x3 :: Int -> Int -> (Int, Int) -> [Element] -> [(Int, Int)]
getFreeCells3x3 n m (x, y) env =  [(a, b) |(a,b) <- [(x, y - 1),(x - 1, y - 1),(x - 1, y),(x - 1, y + 1),(x, y + 1),(x + 1, y + 1),(x + 1, y),(x + 1, y -1) ], isValidMove n m (a, b) env]


isInCrib :: (Int, Int) -> [Element] -> Bool
isInCrib (x, y) env = let elements = getElementAt env (x, y) in findCrib elements

findCrib :: [Element] -> Bool
findCrib [] = False
findCrib (Crib (_, _) : rest) = True
findCrib (_ : rest) = findCrib rest

findObstacle :: [Element] -> Bool
findObstacle [] = False
findObstacle (Obstacle (_, _) : rest) = True
findObstacle (_ : rest) = findObstacle rest

onDirt :: [Element] -> Bool
onDirt [] = False
onDirt (Dirt (_, _) : rest) = True
onDirt (_ : rest) = onDirt rest

getDirtCount :: Int -> Int
getDirtCount 1 = 1
getDirtCount 2 = 3
getDirtCount _ = 6

getChildren :: [Element] -> [Element] -> [Element]
getChildren [] env = []
getChildren (Child (x, y) isCarried : rest) env = 
    if isCarried || isInCrib (x, y) env then getChildren rest env else Child (x, y) isCarried : getChildren rest env
getChildren (_ : rest) env = getChildren rest env

getRobots :: [Element] -> [Element] -> [Element]
getRobots [] env = []
getRobots (Robot (x, y) c : rest) env = Robot (x, y) c : getRobots rest env
getRobots (_ : rest) env = getRobots rest env

isElementIn :: [Element] -> String -> Bool
isElementIn [] _ = False
isElementIn (el : cells) name = isElement el name || isElementIn cells name

isElement :: Element -> String -> Bool
isElement (Child _ _) "Child" = True
isElement (Robot _ _) "Robot" = True
isElement (Dirt _) "Dirt" = True
isElement (Obstacle _) "Obstacle" = True
isElement (Crib _) "Crib" = True
isElement _ _ = False

boardPrint :: [Element] -> Matrix String -> Matrix String
boardPrint [] sMatrix = sMatrix
boardPrint (Dirt (a, b) : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "D" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "D"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Obstacle (a, b) : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "O" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "O"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Crib (a, b) : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "C" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "C"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Child (a, b) c : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "B" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "B"
          nsMatrix = setElem nelem (a, b) sMatrix
       in boardPrint env nsMatrix
boardPrint (Robot (a, b) c : env) sMatrix =
  if sMatrix ! (a, b) == "---"
    then
      let nsMatrix = setElem "R" (a, b) sMatrix
       in boardPrint env nsMatrix
    else
      let elem = sMatrix ! (a, b)
          nelem = elem ++ "R"
          nsMatrix = setElem nelem (a, b) sMatrix
        in boardPrint env nsMatrix
----------------------