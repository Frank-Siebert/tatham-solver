module Game.Puzzle.Towers where

import Data.List(intercalate,group,sort,transpose)

type LatinSquare = [[Box]]

type Box = [Int]

latinSize :: LatinSquare -> Int
latinSize sq = maximum $ length sq:map length sq

type RowConstraint = ([Int] -> Bool)
type TowerEdgeIdx = (Bool,Bool,Int) -- directed by middle Bool
type EdgeIdx = (Bool,Int) -- undirected
data Constraint = Constraint String EdgeIdx RowConstraint

data Edge = L | R | T | B

edge :: Edge -> Int -> TowerEdgeIdx
edge L x = (False,False,x)
edge R x = (False,True ,x)
edge T x = (True ,False,x)
edge B x = (True ,True ,x)

permutePossible :: [Box] -> [[Int]]
permutePossible = filter latin . sequenceA where
    latin = all (==1). map length . group . sort

latinRow :: Int -> RowConstraint
latinRow  s = (\x -> sort x==[1..s])

towerRow :: Bool -> Int -> RowConstraint
towerRow rev vis = (==vis) . length . group . scanl1 max . applyWhen rev reverse

applyWhen :: Bool -> (a -> a) -> (a -> a)
applyWhen True  f = f
applyWhen False f = id

-- lenses would be nice here
withBlock :: EdgeIdx -> ([Box] -> [Box]) -> LatinSquare -> LatinSquare
withBlock (isTranspose,ix) f =
        opttranspose . (\xs -> [ applyWhen (ix==i) f x | (i,x) <- zip [1..] xs]) . opttranspose
    where
        opttranspose = applyWhen isTranspose transpose
-- the 1 in [1..] is the reason why indexes in tower start with 1, not 0

parse :: String -> LatinSquare
parse x = let (ss:ls) = lines x
              s = read ss
              bar l = map possible (take s (l++repeat ' '))
              possible ' ' = [1..s]
              possible  x  = [(read [x])]
           in map bar ls

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = undefined

everywhere :: a -> [a] -> [[a]]
everywhere p [] = [[p]]
everywhere p xxs@(x:xs) = [p:xxs]++map (x:) (everywhere p xs)

towerC :: Int -> (Bool,Bool,Int) -> Constraint
towerC x choice@(vert,isReverse,ix) = Constraint ("tower "++show choice++show x) (vert,ix) (towerRow isReverse x)

fixByConstraint :: RowConstraint -> [Box] ->  [Box]
fixByConstraint c  =
     map (map head . group . sort) . transpose . filter c . permutePossible

iterateStable :: (Eq a) => (a -> a) -> a -> [a]
iterateStable f x = let res = iterate f x
                        zpt = zip res (tail res)
                     in map snd . takeWhile (uncurry (/=)) $ zpt

tower :: Edge -> Int -> Int -> Constraint
tower e idx tw = towerC tw (edge e idx)

extracalate :: [a] -> [[a]] -> [a]
extracalate sep xs = sep ++ intercalate sep xs ++ sep

prettify :: LatinSquare -> String
prettify sq =
    let s = latinSize sq
        boxWidth = 2*s - 1 -- (ceiling . sqrt . fromIntegral $ s)*2-1
        horline = extracalate "+" (replicate s (replicate boxWidth '-')) ++"\n"
        rows = map ((++"\n") . extracalate "|" . map (take boxWidth . (++repeat ' ') . intercalate " " . map show)) sq
     in extracalate horline rows

applyConstraint :: Constraint -> (String, LatinSquare -> LatinSquare)
applyConstraint (Constraint msg edgeIdx rowConstraint) =
     (msg,withBlock edgeIdx (fixByConstraint rowConstraint))

applyCyclicUntilFixed :: (Eq a) => [(b,a -> a)] -> a -> [(b,a)]
applyCyclicUntilFixed allfs oldx = go allfs oldx where
    go [] x | x == oldx = []
            | otherwise = applyCyclicUntilFixed allfs x
    go ((m,f):fs) x | f_x == x  =  go fs x
                | otherwise = (m,f_x) : go fs (f_x) where f_x = f x

latins :: Int -> [Constraint]
latins n = [Constraint ("latin "++show i++if v then " vert" else " hor")  (v,i) (latinRow n) | i <- [1 .. n], v <- [False,True]]

playthrough :: ([Constraint],LatinSquare) -> [(String,LatinSquare)]
playthrough (cs,ls) = applyCyclicUntilFixed (map applyConstraint cs) ls
