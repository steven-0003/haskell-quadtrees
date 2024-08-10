--Quadtree Data type
data Quadtree a = Leaf a
            | Node (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a)
            deriving (Eq, Show)

--Colour data type
data Colour = White | Black
            deriving (Eq, Show)

--Returns a black leaf and ignores the size parameter
--Int -> The size
--Quadtree Colour -> A leaf node of the colour black
allBlack :: Int -> Quadtree Colour
allBlack n
        | n <= 0 = error "Cannot have a Quadtree of size 0 or less"
        | otherwise = Leaf Black

--Returns a white leaf and ignores the size parameter
--Int -> The size
--Quadtree Colour -> A leaf node of the colour white
allWhite :: Int -> Quadtree Colour
allWhite n
        | n <= 0 = error "Cannot have a Quadtree of size 0 or less"
        | otherwise = Leaf White

--Takes 4 quadtrees as a paramater and returns the quadtree with the parameters in a clockwise order
--      clockwise a b c d (where a, b, c and d are quadtrees) =
--
--      ____________________
--     |         |          |
--     |         |          |
--     |    a    |     b    |
--     |         |          |
--     |_________|__________|
--     |         |          |
--     |    d    |     c    |
--     |         |          |
--     |_________|__________|
--

clockwise :: Quadtree Colour -> Quadtree Colour -> Quadtree Colour -> Quadtree Colour -> Quadtree Colour
clockwise a b c d = Node a b d c


--Takes 4 quadtrees as a paramater and returns the quadtree with the parameters in an anticlockwise order
--      anticlockwise a b c d (where a, b, c and d are quadtrees) =
--
--      ____________________
--     |         |          |
--     |         |          |
--     |    a    |     d    |
--     |         |          |
--     |_________|__________|
--     |         |          |
--     |    b    |     c    |
--     |         |          |
--     |_________|__________|
--
anticlockwise :: Quadtree Colour -> Quadtree Colour -> Quadtree Colour -> Quadtree Colour -> Quadtree Colour
anticlockwise a b c d = Node a d b c

--Takes a quadtree and returns it as a list of colourAtPos
-- qtToList 
--      ____________________
--     |         |          |
--     |         |          |
--     |    B    |     W    |
--     |         |          |
--     |_________|__________|
--     |         |          |
--     |    B    |     W    |
--     |         |          |
--     |_________|__________|
--
-- where B and W represent the colours black and white respectively
-- return [Black, White, Black, White]
qtToList :: Quadtree Colour -> [Colour]
qtToList (Leaf a) = [a]
qtToList (Node nw ne sw se) = qtToList nw ++ qtToList ne ++ qtToList sw ++ qtToList se

--removes the head of the list
--used instead of tail to avoid errors on empty lists
removeHead :: [a] -> [a]
removeHead [] = []
removeHead (x:xs) =  xs

--removes the last item of the list
removeLast :: [a] -> [a]
removeLast [] = []
removeLast xs = init xs

--Returns the sub quadtrees inside a quadtree
--Int -> The depth that we need to go down to (the quadtree itself is at depth 0)
--[Int] -> A list of positions that we need to go to, in which each position takes the integer of
--         1, 2, 3, 4 which represents the nw, ne, sw, se nodes respectively
--Quadtree Colour -> The quadtree that we are searching
--E.g. qtDepth 2 [1, 4] 
--                    ____________________
--                   |    |    |          |
--                   |  B | W  |          |
--                   |____|____|     W    |
--                   |  W | B  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    B    |     W    |
--                   |         |          |
--                   |_________|__________| 
-- returns [Leaf(Black)]              
qtDepth :: Int -> [Int] -> Quadtree Colour -> [Quadtree Colour]
qtDepth d _ _
        |d < 0 = []
qtDepth _ _ (Leaf a) = [Leaf a]
qtDepth _ n (Node nw ne sw se)
        |null n = [Node nw ne sw se]
qtDepth d (n:ns) (Node nw ne sw se)
        |d == 0 = [Node nw ne sw se]
        |n == 1 = qtDepth (d-1) ns nw
        |n == 2 = qtDepth (d-1) ns ne
        |n == 3 = qtDepth (d-1) ns sw
        |n == 4 = qtDepth (d-1) ns se
        |otherwise = []

--Extracts a quadtree from a list of quadtrees
--We assume that the list has only one element, to supplement the use of qtDepth and other functions
extractFromList :: [Quadtree Colour] -> Quadtree Colour
extractFromList [] = error "Nothing in List"
extractFromList [a] = a

--Converts a quadtree to a list of positions
--Int -> The depth that we need to go down to (the quadtree itself is at depth 0)
--[Int] -> A list of positions that we need to go to, in which each position takes the integer of
--         1, 2, 3, 4 which represents the nw, ne, sw, se nodes respectively
--Quadtree Colour -> The quadtree that we are searching
--[[Int]] -> A 2D list of positions in which the first integer of every list represents the depth and the rest represents the positions
--The initial call to positions would be
--positions 0 [] 
--                    ____________________
--                   |    |    |          |
--                   |  B | W  |          |
--                   |____|____|     W    |
--                   |  W | B  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    B    |     W    |
--                   |         |          |
--                   |_________|__________|
--
-- returns [[2,1,1],[2,1,2],[2,1,3],[2,1,4],[1,2],[1,3],[1,4]] 
positions :: Int -> [Int] -> Quadtree Colour -> [[Int]]
positions d n (Leaf _) = [d:n]
positions d n (Node nw ne sw se) = positions (d+1) (n++[1]) nw ++ positions (d+1) (n++[2]) ne ++ positions (d+1) (n++[3]) sw ++ positions (d+1) (n++[4]) se

--Changes the colour of a leaf in a quadtree
--[Int] -> A list of positions
--Colour -> The colour that we need to change to
--Quadtree Colour -> The quadtree that we are applying the colour change to
changeColour :: [Int] -> Colour -> Quadtree Colour -> Quadtree Colour
changeColour _ Black (Leaf _) = Leaf Black
changeColour _ White (Leaf _) = Leaf White
changeColour [] _ _ = error "No positions provided"
changeColour (x:xs) c (Node nw ne sw se)
        |x == 1 = Node (changeColour xs c nw) ne sw se
        |x == 2 = Node nw (changeColour xs c ne) sw se
        |x == 3 = Node nw ne (changeColour xs c sw) se
        |x == 4 = Node nw ne sw (changeColour xs c se)
        |otherwise = error "Invalid position"

--Counts the number of black colours in a list of colours
countBlacks :: [Colour] -> Int
countBlacks [] = 0
countBlacks (x:xs)
        |x == Black = 1 + countBlacks xs
        |otherwise = countBlacks xs

--Counts the number of white colours in a list of colours
countWhites :: [Colour] -> Int
countWhites [] = 0
countWhites (x:xs)
        |x == White = 1 + countWhites xs
        |otherwise = countWhites xs

--Gets the colour of a leaf node
--[Int] -> A list of positions, where the first integer is the depth
--Quadtree Colour -> The quadtree that we are searching
colourAtPos :: [Int] -> Quadtree Colour -> Colour
colourAtPos _ (Leaf a) = a
colourAtPos [] _= error "No positions provided"
colourAtPos (x:xs) (Node nw ne sw se) = colourAtPos [] (extractFromList (qtDepth x xs (Node nw ne sw se)))

--Selects a colour for a specific position, which is used after the neighbours of a leaf are found
--[Colour] -> A list of Colours
--[Int] -> A list of positions, where the first integer is the depth
--Quadtree Colour -> The quadtree that we are searching
--If there are more black neighbours than white, we return black
--If there are more white neighbours than black, we return white
--If the number of black and white neighbours are equal, we will use colourAtPos to get the colour of the current Leaf
selectColour :: [Colour] -> [Int] -> Quadtree Colour -> Colour
selectColour [] _ _ = error "No colours to select from"
selectColour _ [] _ = error "No positions provided"
selectColour _ _ (Leaf a) = a
selectColour cs (x:xs) (Node nw ne sw se)
        |countBlacks cs > countWhites cs = Black
        |countBlacks cs < countWhites cs = White
        |otherwise = colourAtPos (x:xs) (Node nw ne sw se)
 
--Gets the nodes at the leftmost side of the quadtree and returns it as a list of colours
--                    ____________________
-- leftNodes         |    |    |          |
--                   |  B | W  |          |
--                   |____|____|     W    |
--                   |  W | B  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    B    |     W    |
--                   |         |          |
--                   |_________|__________| 
-- = [Black, White, Black]
leftNodes :: [Quadtree Colour] -> [Colour]
leftNodes [] = []
leftNodes [Leaf a] = [a]
leftNodes [Node nw ne sw se] = leftNodes [nw] ++ leftNodes [sw]

--Gets the nodes at the rightmost side of the quadtree and returns it as a list of colours
--                    ____________________
-- rightNodes        |    |    |          |
--                   |  B | W  |          |
--                   |____|____|     W    |
--                   |  W | B  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    B    |     W    |
--                   |         |          |
--                   |_________|__________| 
-- = [White, White]
rightNodes :: [Quadtree Colour] -> [Colour]
rightNodes [] = []
rightNodes [Leaf a] = [a]
rightNodes [Node nw ne sw se] = rightNodes [ne] ++ rightNodes [se]

--Gets the nodes at the upmost side of the quadtree and returns it as a list of colours
--                    ____________________
-- topNodes          |    |    |          |
--                   |  B | W  |          |
--                   |____|____|     W    |
--                   |  W | B  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    B    |     W    |
--                   |         |          |
--                   |_________|__________| 
-- = [Black, White, White]
topNodes :: [Quadtree Colour] -> [Colour]
topNodes [] = []
topNodes [Leaf a] = [a]
topNodes [Node nw ne sw se] = topNodes [nw] ++ topNodes [ne]

--Gets the nodes at the downmost side of the quadtree and returns it as a list of colours
--                    ____________________
--bottomNodes        |    |    |          |
--                   |  B | W  |          |
--                   |____|____|     W    |
--                   |  W | B  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    B    |     W    |
--                   |         |          |
--                   |_________|__________| 
-- = [Black, White]
bottomNodes :: [Quadtree Colour] -> [Colour]
bottomNodes [] = []
bottomNodes [Leaf a] = [a]
bottomNodes [Node nw ne sw se] = bottomNodes [sw] ++ bottomNodes [se]

--Checks if we can go up from a position
--[Int] -> A list of positions
--We can't go up in the case that is at the upmost part of the quadtree, i.e., it is only in the top left
--or top right part of the quadtree and its sub quadtrees
canGoUp :: [Int] -> Bool
canGoUp [] = False
canGoUp (x:xs)
        |x == 1 || x == 2 = canGoUp xs
        |otherwise = True

--Checks if we can go down from a position
--[Int] -> A list of positions
--We can't down up in the case that is at the downmost part of the quadtree, i.e., it is only in the bottom left
--or bottom right part of the quadtree and its sub quadtrees
canGoDown :: [Int] -> Bool
canGoDown [] = False
canGoDown (x:xs)
        |x == 3 || x == 4 = canGoDown xs
        |otherwise = True

--Checks if we can go left from a position
--[Int] -> A list of positions
--We can't go left in the case that is at the leftmost part of the quadtree, i.e., it is only in the top left
--or bottom left part of the quadtree and its sub quadtrees
canGoLeft :: [Int] -> Bool
canGoLeft [] = False
canGoLeft (x:xs)
        |x == 1 || x == 3 = canGoLeft xs
        |otherwise = True

--Checks if we can go right from a position
--[Int] -> A list of positions
--We can't go right in the case that is at the rightmost part of the quadtree, i.e., it is only in the top right
--or bottom right part of the quadtree and its sub quadtrees
canGoRight :: [Int] -> Bool
canGoRight [] = False
canGoRight (x:xs)
        |x == 2 || x == 4 = canGoRight xs
        |otherwise = True
        
--Gets the position of the leaf above it
--If the position of the current leaf is at the bottom left or bottom right of its quadtree, then we only need to go up
--by changing the value of the last integer
--Otherwise, it means we have to go to a different quadtree
positionAbove :: [Int] -> [Int]
positionAbove [] = []
positionAbove x
        |last x == 3 = removeLast x ++ [1]
        |last x == 4 = removeLast x ++ [2]
        |last x == 1 = positionAbove (removeLast x) ++ [3]
        |last x == 2 = positionAbove (removeLast x) ++ [4]

--Gets the position of the leaf below it
--If the position of the current leaf is at the top left or top right of its quadtree, then we only need to go down
--by changing the value of the last integer
--Otherwise, it means we have to go to a different quadtree
positionBelow :: [Int] -> [Int]
positionBelow [] = []
positionBelow x
        |last x == 1 = removeLast x ++ [3]
        |last x == 2 = removeLast x ++ [4]
        |last x == 3 = positionBelow (removeLast x) ++ [1]
        |last x == 4 = positionBelow (removeLast x) ++ [2]

--Gets the position of the leaf to the left
--If the position of the current leaf is at the top right or bottom right of its quadtree, then we only need to go left
--by changing the value of the last integer
--Otherwise, it means we have to go to a different quadtree
positionLeft :: [Int] -> [Int]
positionLeft [] = []
positionLeft x
        |last x == 2 = removeLast x ++ [1]
        |last x == 4 = removeLast x ++ [3]
        |last x == 1 = positionLeft (removeLast x) ++ [2]
        |last x == 3 = positionLeft (removeLast x) ++ [4]

--Gets the position of the leaf to the right
--If the position of the current leaf is at the top left or bottom left of its quadtree, then we only need to go right
--by changing the value of the last integer
--Otherwise, it means we have to go to a different quadtree
positionRight :: [Int] -> [Int]
positionRight [] = []
positionRight x
        |last x == 1 = removeLast x ++ [2]
        |last x == 3 = removeLast x ++ [4]
        |last x == 2 = positionRight (removeLast x) ++ [1]
        |last x == 4 = positionRight (removeLast x) ++ [3]

--Gets the quadtree that is above the leaf we are looking at
--[Int] -> A list of positions, where the first integer is the depth
--Quadtree Colour -> The quadtree that we are searching
--If it is the case that we can't go up, an empty list is returned
--Otherwise, we will call qtDepth using positionAbove as the parameter for the position
--Even if above the current leaf, there is a leaf at a lower depth (has a bigger size),
--the base case of qtDepth will still return that leaf
goUp :: [Int] -> Quadtree Colour -> [Quadtree Colour]
goUp [] _ = []
goUp _ (Leaf _) = []
goUp (x:xs) (Node nw ne sw se)
        |not (canGoUp xs) = []
        |otherwise = qtDepth x (positionAbove xs) (Node nw ne sw se)

--Gets the quadtree that is below the leaf we are looking at
--[Int] -> A list of positions, where the first integer is the depth
--Quadtree Colour -> The quadtree that we are searching
--If it is the case that we can't go down, an empty list is returned
--Otherwise, we will call qtDepth using positionBelow as the parameter for the position
--Even if below the current leaf, there is a leaf at a lower depth (has a bigger size),
--the base case of qtDepth will still return that leaf
goDown :: [Int] -> Quadtree Colour -> [Quadtree Colour]
goDown [] _ = []
goDown _ (Leaf _) = []
goDown (x:xs) (Node nw ne sw se)
        |not (canGoDown xs) = []
        |otherwise = qtDepth x (positionBelow xs) (Node nw ne sw se)

--Gets the quadtree that is to the left of the leaf we are looking at
--[Int] -> A list of positions, where the first integer is the depth
--Quadtree Colour -> The quadtree that we are searching
--If it is the case that we can't go left, an empty list is returned
--Otherwise, we will call qtDepth using positionLeft as the parameter for the position
--Even if to the left of the current leaf, there is a leaf at a lower depth (has a bigger size),
--the base case of qtDepth will still return that leaf
goLeft :: [Int] -> Quadtree Colour -> [Quadtree Colour]
goLeft [] _ = []
goLeft _ (Leaf _) = []
goLeft (x:xs) (Node nw ne sw se)
        |not (canGoLeft xs) = []
        |otherwise = qtDepth x (positionLeft xs) (Node nw ne sw se)
         
--Gets the quadtree that is to the right of the leaf we are looking at
--[Int] -> A list of positions, where the first integer is the depth
--Quadtree Colour -> The quadtree that we are searching
--If it is the case that we can't go right, an empty list is returned
--Otherwise, we will call qtDepth using positionRight as the parameter for the position
--Even if to the right of the current leaf, there is a leaf at a lower depth (has a bigger size),
--the base case of qtDepth will still return that leaf
goRight :: [Int] -> Quadtree Colour -> [Quadtree Colour]
goRight [] _ = []
goRight _ (Leaf _) = []
goRight (x:xs) (Node nw ne sw se)
        |not (canGoRight xs) = []
        |otherwise = qtDepth x (positionRight xs) (Node nw ne sw se)

--Gets the neighbours of a leaf
--[Int] -> The position of the leaf in the sub quadtree
--[Int] -> The position of the leaf in the whole of the quadtree
--Quadtree Colour -> The sub quadtree
--Quadtree Colour -> The whole quadtree
--
--We will recursively call neighbours until we get the case that we have a sub quadtree in which the position of the leaf node
--can only be in one of the 4 nodes in the quadtree, i.e., getting a further sub quadtree would result in a leaf, which we do not want
--as a leaf cannot have any neighbours
--
--If the leaf is in the top left, we can call leftNodes on the top right and topNodes on the bottom left,
--as these nodes are in the same quadtree. We will then call goUp and goLeft as this requires going to a different quadtree
--in which we call bottomNodes and rightNodes respectively
--
--If the leaf is in the top right, we can call rightNodes on the top left and topNodes on the bottom right,
--as these nodes are in the same quadtree. We will then call goUp and goRight as this requires going to a different quadtree
--in which we call bottomNodes and leftNodes respectively
--
--If the leaf is in the bottom left, we can call bottomNodes on the top left and leftNodes on the bottom right,
--as these nodes are in the same quadtree. We will then call goDown and goLeft as this requires going to a different quadtree
--in which we call topNodes and rightNodes respectively
--
--If the leaf is in the bottom right, we can call rightNodes on the bottom left and bottomNodes on the top right,
--as these nodes are in the same quadtree. We will then call goDown and goRight as this requires going to a different quadtree
--in which we call topNodes and leftNodes respectively
neighbours :: [Int] -> [Int] -> Quadtree Colour -> Quadtree Colour -> [Colour]
neighbours [] _ _ _ = []
neighbours _ [] _ _= []
neighbours _ _ _ (Leaf _) = []
neighbours (x:xs) y (Leaf _) z
        | x == 0 = []
        | otherwise = neighbours ((x-1):xs) y (extractFromList(qtDepth (x-1) xs z)) z
neighbours (x:xs) y (Node nw ne sw se) z
        |x == 0 = []
        |xs == [1] = leftNodes [ne] ++ topNodes [sw] ++ bottomNodes (goUp y z) ++ rightNodes (goLeft y z)
        |xs == [2] = rightNodes [nw] ++ topNodes [se] ++ bottomNodes (goUp y z) ++ leftNodes (goRight y z)
        |xs == [3] = bottomNodes [nw] ++ leftNodes [se] ++ topNodes (goDown y z) ++ rightNodes (goLeft y z)
        |xs == [4] = rightNodes [sw] ++ bottomNodes [ne] ++ topNodes (goDown y z) ++ leftNodes (goRight y z)
        |otherwise = neighbours ((x-1):[last xs]) y (extractFromList(qtDepth (x-1) (removeLast xs) z)) z

--Goes through every position in a quadtree, and returns the quadtree with the colour at the position changed
--[[Int]] -> List of positions in a quadtree
--Quadtree Colour -> The quadtree with the colours changed
--Quadtree Colour -> The initial quadtree
--This will be recursively called until we get to just one leaf left
goThroughPositions :: [[Int]] -> Quadtree Colour -> Quadtree Colour -> Quadtree Colour
goThroughPositions [] _ _= error "No positions provided"
goThroughPositions(x:xs) (Node nw ne sw se) originalQT
        |null xs =  changeColour (removeHead x) (selectColour (neighbours x x originalQT originalQT) x originalQT) 
        (Node nw ne sw se)
        |otherwise = goThroughPositions xs (changeColour (removeHead x) (selectColour 
        (neighbours x x originalQT originalQT) x originalQT) (Node nw ne sw se)) originalQT

--Blurs a quadtree by looking at the neighbours of every leaf
--In the base case, a single leaf returns itself as a leaf does not have any neighbours
--We will then call goThroughPositions on the list of positions
--E.g.                ____________________
--blur               |    |    |          |
--                   |  B | W  |          |
--                   |____|____|     W    |
--                   |  W | B  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    B    |     W    |
--                   |         |          |
--                   |_________|__________| 
--
--                    ____________________
--  =                |    |    |          |
--                   |  W | B  |          |
--                   |____|____|     W    |
--                   |  B | W  |          |
--                   |____|____|__________|
--                   |         |          |
--                   |    W    |     W    |
--                   |         |          |
--                   |_________|__________| 
blur :: Quadtree Colour -> Quadtree Colour
blur (Leaf a) = Leaf a
blur (Node nw ne sw se) = goThroughPositions (positions 0 [] (Node nw ne sw se)) (Node nw ne sw se) (Node nw ne sw se)