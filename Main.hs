module Main where

import System.IO.Unsafe  -- be careful!                                          
import System.Random 

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
main =  print (generateDirt 5 5 [Child (3, 1) False, Child (2,4) False] [Obstacle (2,1), Obstacle (4,1),Obstacle (3,0), Obstacle (3,2), Obstacle (3,3),Child (3,1) False, Child (2,4) False] 5) 
-- main =  print (removeElement (0,2) [(1,2), (3,4)]) 

initialize :: Int -> Int -> Int -> Int -> Int -> Int -> [Element]
initialize n m children robots dirt obstacles = 
    let env2 =  putChildren n m children []
        env3 = putRobots n m robots env2
        env4 = putObstacles n m obstacles env3
    in putDirt n m dirt env4

putCrib :: Int -> Int -> Int -> [Element]
putCrib n m children = 
    let x = randomInt 0 (n - 1)
        y = randomInt 0 (m - 1) 
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
    let x = randomInt 0 (n - 1)
        y = randomInt 0 (m - 1)
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Child (x, y) False] in putChildren n m (childCount - 1) newEnv
            else putChildren n m childCount env

putRobots :: Int -> Int -> Int -> [Element] -> [Element]
putRobots _ _ 0 env = env
putRobots n m robotCount env =
    let x = randomInt 0 (n - 1)
        y = randomInt 0 (m - 1)
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Robot (x, y) False] in putRobots n m (robotCount - 1) newEnv
            else putRobots n m robotCount env

putObstacles :: Int -> Int -> Int -> [Element] -> [Element]
putObstacles _ _ 0 env = env
putObstacles n m obsCount env =
    let x = randomInt 0 (n - 1)
        y = randomInt 0 (m - 1)
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Obstacle (x, y)] in putObstacles n m (obsCount - 1) newEnv
            else putObstacles n m obsCount env

putDirt :: Int -> Int -> Int -> [Element] -> [Element]
putDirt _ _ 0 env = env
putDirt n m dirtCount env =
    let x = randomInt 0 (n - 1)
        y = randomInt 0 (m - 1)
        in if isValidMove n m (x, y) env
            then let newEnv = env ++ [Dirt (x, y)] in putDirt n m (dirtCount - 1) newEnv
            else putDirt n m dirtCount env



moveChildren :: Int -> Int -> [Element] -> [Element] -> [Element]
moveChildren n m currentEnv [] = currentEnv
moveChildren n m currentEnv (Child (x, y) isCarried : elements)  = 
    let direction = randomInt 0 4
        child = Child (x, y) isCarried
    in case direction of
        0 -> moveChildren n m currentEnv elements --Doesn't move
        2 -> if isInBoard x (y-1) n m then (let newEnv = moveChild n m currentEnv child (0, -1) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Left
        1 -> if isInBoard x (y+1) n m then (let newEnv = moveChild n m currentEnv child (0,  1) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Right
        4 -> if isInBoard (x-1) y n m then (let newEnv = moveChild n m currentEnv child (-1, 0) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Top
        3 -> if isInBoard (x+1) y n m then (let newEnv = moveChild n m currentEnv child (1,  0) in moveChildren n m newEnv elements) else moveChildren n m currentEnv elements --Bottom

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

generateDirt :: Int -> Int -> [Element] -> [Element] -> Int -> [Element]
generateDirt n m [] env dirtCount = env
generateDirt n m _ env 0 = env
generateDirt n m (Child (x, y) _ : children) env dirtCount = 
    let freeCells = getFreeCells3x3 n m (x, y) env
        freeCellsCount = length freeCells
        -- minCount = minimum [dirtCount, freeCellsCount]
        currentDirtCount = randomInt 0 dirtCount
        newEnv = generateDirtInGrid freeCells currentDirtCount env
    in generateDirt n m children newEnv (dirtCount - currentDirtCount)

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

    


randomInt :: Int -> Int -> Int 
randomInt min max = unsafePerformIO (getStdRandom (randomR (min, max)))

isChild :: Element -> Bool
isChild (Child (a, b) c) = True 
isChild _ = False

getRow :: Element -> Int
getRow (Dirt (x, y)) = x
getRow (Obstacle (x, y)) = x
getRow (Crib (x, y)) = x
getRow (Child (x, y) c) = x
getRow (Robot (x, y) c) = x

getColumn :: Element -> Int
getColumn (Dirt (x, y)) = y
getColumn (Obstacle (x, y)) = y
getColumn (Crib (x, y)) = y
getColumn (Child (x, y) c) = y
getColumn (Robot (x, y) c) = y

isOccupied :: (Int, Int) -> [Element] -> Bool
isOccupied (x, y) [] = False
isOccupied (x, y) (el : rest) = 
     let elx = getRow el
         ely = getColumn el
         in elx == x && ely == y
         || isOccupied (x, y) rest

isInBoard :: Int -> Int -> Int -> Int-> Bool
isInBoard x y n m = 0 <= x && x < n && 0 <= y && y < m

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
  if ( let re = getRow x
           ce = getColumn x
        in re == r && ce == c
     )
    then y ++ [x]
    else y
    where y = []

getFreeCells3x3 :: Int -> Int -> (Int, Int) -> [Element] -> [(Int, Int)]
getFreeCells3x3 n m (x, y) env = 
    let c  = [(x, y) |isValidMove n m (x, y) env]
        l  = if isValidMove n m (x, y - 1)     env then c  ++ [(x, y - 1)]     else c 
        tl = if isValidMove n m (x - 1, y - 1) env then l  ++ [(x - 1, y - 1)] else l 
        t  = if isValidMove n m (x - 1, y)     env then tl ++ [(x - 1, y)]     else tl
        tr = if isValidMove n m (x - 1, y + 1) env then t  ++ [(x - 1, y + 1)] else t 
        r  = if isValidMove n m (x, y + 1)     env then tr ++ [(x, y + 1)]     else tr
        br = if isValidMove n m (x + 1, y + 1) env then r  ++ [(x + 1, y + 1)] else r 
        b  = if isValidMove n m (x + 1, y)     env then br ++ [(x + 1, y)]     else br
        bl = if isValidMove n m (x + 1, y -1)  env then b  ++ [(x + 1, y -1)]  else b 
        in bl
        