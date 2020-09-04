import Control.Applicative(liftA2)
import Data.Foldable(fold)
import Data.List((\\),intercalate,group,nub,sort,transpose)
--import Data.List.Ordered(unionAll)

type LatinSquare = [[Box]]

type Box = [Int]

isFixed :: Box -> Bool
isFixed [_] = True
isFixed  _  = False

latinSize :: LatinSquare -> Int
latinSize sq = maximum $ length sq:map length sq

type RowConstraint = ([Int] -> Bool)
type TowerEdgeIdx = (Bool,Bool,Int) -- directed by middle Bool
type EdgeIdx = (Bool,Int) -- undirected
data Constraint = Constraint String EdgeIdx RowConstraint -- +String?

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

-- first Bool: true row, not line. second bool WAS: direction right or up
chooseBlock :: EdgeIdx -> LatinSquare -> [Box]
chooseBlock (True ,ix) sq = chooseBlock (False,ix) (transpose sq)
chooseBlock (False,ix) sq = sq !! ix

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
              bar l = map baz (take s (l++repeat ' '))
              baz ' ' = [1..s]
              baz  x  = [(read [x])]  
           in map bar ls

isSolved :: LatinSquare -> Bool
isSolved sq = all (all isFixed) sq

solves :: [Step] -> LatinSquare -> LatinSquare
solves steps ls | all (\step -> applyStep step ls == ls) steps = ls
solves steps ls = solves steps (applyStep (foldl1 (.) steps) ls)

type Step = [Box] -> [Box]
deleteFixed :: Step
deleteFixed bs = deletePs fixed bs
   where fixed = [n | [n] <- bs]

-- step helper function
deletePs :: [Int] -> [Box] -> [Box]
deletePs d = map (\\d)

applyStep :: Step -> LatinSquare -> LatinSquare
applyStep step = transpose . map step . transpose . map step 

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = undefined

everywhere :: a -> [a] -> [[a]]
everywhere p [] = [[p]]
everywhere p xxs@(x:xs) = [p:xxs]++map (x:) (everywhere p xs)

example :: LatinSquare
example = parse "3\n1\n 2\n  3"

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

fld :: (Traversable t, Applicative f) => (t b -> c) -> t (f b) -> (f c)
fld combiner stuff = combiner <$> sequenceA stuff

myfld :: [a -> Bool] -> a -> Bool
myfld constraints x = all ($x) constraints
--myfld constraints x = fld and constraints x

-- [a -> Bool] -> (a -> Bool)
-- t (f b) -> (f b) , t ~ [], b ~ Bool, f ~ (->) a
-- (t b -> c) -> t (f b) -> (f c)

extracalate :: [a] -> [[a]] -> [a]
extracalate sep xs = sep ++ intercalate sep xs ++ sep

prettyPrint :: LatinSquare -> String
prettyPrint sq =
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
    go ((m,f):fs) x | f_x == x  =  go fs x -- f_x hier raus, debug only!
                | otherwise = (m,f_x) : go fs (f_x) where f_x = f x

hardWith3 :: ([Constraint], LatinSquare)
hardWith3 = ([tower L 1 2,
              tower B 1 1,
              tower R 3 2]++latins 3,replicate 3 . replicate 3 $ [1..3])

hardWith6 :: ([Constraint], LatinSquare)
hardWith6 =([tower L 1 2,
             tower L 2 3,
             tower L 3 1,
             tower L 4 2,
             tower T 3 3,
             tower T 4 2,
             tower R 2 1,
             tower B 2 1]++latins 6, parse ("6\n"++
                "      \n"++
                "      \n"++
                "   1  \n"++
                "     3\n"++
                "  3   \n"++
                "      "))
                

latins :: Int -> [Constraint]
latins n = [Constraint ("latin "++show i++if v then " vert" else " hor")  (v,i) (latinRow n) | i <- [1 .. n], v <- [False,True]]

playthrough :: ([Constraint],LatinSquare) -> [(String,LatinSquare)]
playthrough (cs,ls) = applyCyclicUntilFixed (map applyConstraint cs) ls

play :: ([Constraint],LatinSquare) -> IO ()
play game = mapM_ (\(msg,sq) -> putStrLn msg >> putStrLn (prettyPrint sq)) (playthrough game)