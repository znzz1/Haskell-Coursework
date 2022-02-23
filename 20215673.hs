-- Three given maps used to test
map1 :: [[Char]]
map1 = ["Z.2","1#.","A.."]
map2 :: [[Char]]
map2 = ["Z.1..","####.","1.3..",".#.#.","A..2."]
map3 :: [[Char]]
map3 = ["..2..Z",".####.","..3...","1#..#.",".#.##1","A..1.."]

-- Check whether one pos on the given map
posOnMap :: [[Char]] -> (Int,Int) -> Bool
posOnMap vss (x,y) = x>=0 && x<length vss
                     && y>=0 && y<length(vss!!x)


getAtPos :: [[Char]] -> (Int,Int) -> Char
getAtPos vss (x,y)  | posOnMap vss (x,y) = (vss!!x)!!y
                    | otherwise          = '#'

-- Set the value at given index position(a) to a new value(b)
setAtIndex :: [a] -> Int -> a -> [a]
setAtIndex (x:xs) 0 b = b:xs
setAtIndex (x:xs) a b = x : setAtIndex xs (a-1) b

-- Update the map, set the value at the given index position to '#'
blockAtPos :: [[Char]] -> (Int, Int) -> [[Char]]
blockAtPos vss (a,b) = setAtIndex vss a (setAtIndex (vss!!a) b '#')

-- Find the index position of start in the given map
findStart :: [[Char]] -> (Int, Int)
findStart vss = head[(a,b)|a<-[0..(length vss -1)],b<-[0..(length (vss!!a) -1)],(vss!!a)!!b=='A']

-- Check whether one index position at given map is legal position
legalPos :: [[Char]] -> (Int, Int) -> Bool
legalPos vss (a,b) | a<0 || b<0                              = False    -- required, used to prevent the program
                   | (a>=length vss) || (b>=length (vss!!a)) = False    -- out of the map
                   | (vss!!a)!!b == '#'                      = False    -- wall n the map
                   | otherwise                               = True

-- Check the next index of given index and position
movePos :: (Int, Int) -> Char -> (Int, Int)
movePos (x,y) 'N' = (x-1,y)
movePos (x,y) 'S' = (x+1,y)
movePos (x,y) 'E' = (x,y+1)
movePos (x,y) 'W' = (x,y-1)

-- List all legal moves of given index position and map
legalMoves :: [[Char]] -> (Int, Int) -> [Char]
legalMoves vss (a,b) = [x|x<-['N','S','E','W'],legalPos vss (movePos (a,b) x)]

-- Get the value at given index position in the given map
valueAtPos :: [[Char]] -> (Int, Int) -> Int
valueAtPos vss (a,b) | (vss!!a)!!b == '1' = 1
                     | (vss!!a)!!b == '2' = 2
                     | (vss!!a)!!b == '3' = 3
                     | otherwise          = 0

-- Find all possible ways, whether to the end or not.
-- Pos and dir used to recursion.
-- Val used to indicate the jewel value. Fes used to indicate whether Imp to the final.
listAll :: [[Char]] -> Int -> ((Int,Int),[Char],Int,Int) -> [((Int,Int),[Char],Int,Int)]
listAll vss fuel (pos,dir,val,fes)| fuel>=0 && (getAtPos vss pos == 'Z') = [(pos,dir,val,1)]    -- base case 0, find the position and does not run out of the fuel
                                  | fuel == 0                            = [(pos,dir,val,0)]    -- base case 1, run out of the fuel
                                  | otherwise                            = concat[listAll (blockAtPos vss pos) (fuel-1) temp|temp<-[((i,j),dir++[np],val+value,fes)|np<-legalMoves vss pos,i<-[0..(length vss -1)],j<-[0..(length (vss!!i) -1)],(i,j)==movePos pos np,value<-[0..3],value==valueAtPos vss (i,j)]]

-- Quicksort a list of tuple ([char],Int) by the second number, from bigger to smaller
quickSort :: [([Char],Int)] -> [([Char],Int)]
quickSort [] = []
quickSort (x:xs) = 
    quickSort bigger ++ [x] ++ quickSort smaller
    where
        bigger = [a|a<-xs,snd a > snd x]
        smaller = [b|b<-xs,snd b <= snd x]


-- Handle the no way found situation, if any case the Imp can not be the final position, return ("",0)
-- From all possible ways, filter possible ways and delete redundant info
-- Use quickSort to sort the filtered list, use head to take the best way       
bestPath :: [[Char]] -> (Int,Int) -> Int -> ([Char],Int)
bestPath vss pos fuel = if not (null [(a2,a3)|(a1,a2,a3,a4)<-listAll vss fuel (pos,"",0,0),a4==1]) then head(quickSort[(a2,a3)|(a1,a2,a3,a4)<-listAll vss fuel (pos,"",0,0),a4==1])
                            else ("",0)

-- Encapsulation                      
play :: [[Char]] -> Int -> ([Char],Int)
play vss = bestPath vss (findStart vss)
