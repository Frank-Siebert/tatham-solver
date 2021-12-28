{-# LANGUAGE OverloadedStrings #-}

module SimpleWeb where

import Data.List(last)
import Web.Scotty
import Game.Puzzle.Towers


--serve :: IO ()
serve = post "/towers" $ 
          do x <- jsonData
             let y = toTower x
             let z = playthrough y
             json (toMyResponse z)
             
toTower :: [[String]] -> ([Constraint],LatinSquare)
toTower x = let size = maximum ( length x : map length x ) - 2
                takeMiddle = drop 1 . take size
                possible "" = [1..size]
                possible s  = read s
             in (makeTowerConstraints T (takeMiddle (head x)) ++
                 makeTowerConstraints B (takeMiddle (last x)) ++
                 makeTowerConstraints L (takeMiddle (map head x)) ++
                 makeTowerConstraints R (takeMiddle (map last x)),
                 (map . map) possible . takeMiddle . map takeMiddle $ x)
                
makeTowerConstraints :: Edge -> [String] -> [Constraint]
makeTowerConstraints e ls = [ tower e i (read x) | (i,x) <- zip [1..] ls, x /= ""] 

toMyResponse :: [(String,LatinSquare)] -> [String]
toMyResponse moves = undefined
