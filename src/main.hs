import Control.Applicative(liftA2)
import Data.Foldable(fold)
import Data.List((\\),group,nub,sort,transpose)
--import Data.List.Ordered(unionAll)

data LatinSquare = LatinSquare [[Box]] deriving (Show,Eq)

type Box = [Int]

isFixed :: Box -> Bool
isFixed [_] = True
isFixed  _  = False

latinSize :: LatinSquare -> Int
latinSize (LatinSquare sq) = maximum $ length sq:map length sq

type RowConstraint = ([Int] -> Bool)
type EdgeIdx = (Bool,Bool,Int)
data Constraint = Constraint EdgeIdx RowConstraint -- +String?

data Edge = L | R | T | B

edge :: Edge -> Int -> EdgeIdx
edge L x = (False,False,x)
edge R x = (False,True ,x)
edge T x = (True ,False,x)
edge B x = (True ,True ,x)

permutePossible :: [Box] -> [[Int]]
permutePossible = filter latin . sequenceA where
    latin = all (==1). map length . group . sort

latinRow :: Int -> RowConstraint
latinRow  s = (\x -> sort x==[1..s])
towerRow :: Int -> RowConstraint
towerRow vis = (==vis) . length . group . scanl1 max

-- first Bool: true row, not line. second bool: direction right or up
chooseBlock :: EdgeIdx -> LatinSquare -> [Box]
chooseBlock (True,fromBelow,ix) (LatinSquare sq) = chooseBlock (False,fromBelow,ix) (LatinSquare (transpose sq))
chooseBlock (False,True,ix) sq = reverse (chooseBlock (False,False,ix) sq)
chooseBlock (False,False,ix) (LatinSquare sq) = sq !! ix

parse :: String -> LatinSquare
parse x = let (ss:ls) = lines x
              s = read ss
              foo = map bar ls
              bar l = map baz (take s (l++repeat ' '))
              baz ' ' = [1..s]
              baz  x  = (read [x])  
           in LatinSquare foo

isSolved :: LatinSquare -> Bool
isSolved (LatinSquare sq) = all (all isFixed) sq

solves :: [Step] -> LatinSquare -> LatinSquare
solves steps ls@(LatinSquare sq) | all (\step -> applyStep step ls == ls) steps = ls
solves steps ls = solves steps (applyStep (foldl1 (.) steps) ls)

type Step = [Box] -> [Box]
deleteFixed :: Step
deleteFixed bs = deletePs fixed bs
   where fixed = [n | [n] <- bs]

-- step helper function
deletePs :: [Int] -> [Box] -> [Box]
deletePs d = map (\\d)

applyStep :: Step -> LatinSquare -> LatinSquare
applyStep step (LatinSquare sq) = LatinSquare (transpose . map step . transpose . map step $ sq)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = undefined

everywhere :: a -> [a] -> [[a]]
everywhere p [] = [[p]]
everywhere p xxs@(x:xs) = [p:xxs]++map (x:) (everywhere p xs)

example :: LatinSquare
example = parse "3\n1\n 2\n  3"

towerC :: Int -> (Bool,Bool,Int) -> Constraint
towerC x choice = Constraint choice (towerRow x)

fixByConstraint :: RowConstraint -> [Box] ->  [Box]
fixByConstraint c  =
     map nub . transpose . filter c . permutePossible 

iterateStable :: (Eq a) => (a -> a) -> a -> [a]
iterateStable f x = let res = iterate f x
                        zpt = zip res (tail res)
                     in map snd . takeWhile (uncurry (/=)) $ zpt

tower :: Edge -> Int -> Int -> Constraint
tower e idx tw = towerC tw (edge e idx)

solve :: [Constraint] -> String -> [LatinSquare]
solve cs inp = let sq = parse inp
                   size = latinSize sq
                   css =undefined
                in iterateStable css sq

fld :: (Traversable t, Applicative f) => (t b -> c) -> t (f b) -> (f c)
fld combiner stuff = combiner <$> sequenceA stuff

myfld :: [a -> Bool] -> a -> Bool
myfld constraints x = all ($x) constraints
--myfld constraints x = fld and constraints x

-- [a -> Bool] -> (a -> Bool)
-- t (f b) -> (f b) , t ~ [], b ~ Bool, f ~ (->) a
-- (t b -> c) -> t (f b) -> (f c)

