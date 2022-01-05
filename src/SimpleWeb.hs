{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module SimpleWeb where

import Control.Monad( forM )
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import Data.List( last )
import Data.Text.Lazy( pack, unpack )
import GHC.Generics
import Web.Scotty

import Game.Puzzle.Towers


--serve :: IO ()
serve = post "/towers" $ 
          do size :: Int <- param "boardsize"
             board <- forM [0::Int .. size+1] $ \y ->
                forM [0::Int .. size+1] $ \x -> param . pack $ "x"++show x++"y"++show y
             liftIO . print $ board
             let g = toTower board
             liftIO . print . snd $ g
             let solution = playthrough g
             json (toMyResponse solution)
             
toTower :: [[String]] -> ([Constraint],LatinSquare)
toTower x = let size = maximum ( length x : map length x ) - 2
                takeMiddle = take size . drop 1
                possible "" = [1..size]
                possible s  = read s
             in (makeTowerConstraints T (takeMiddle (head x)) ++
                 makeTowerConstraints B (takeMiddle (last x)) ++
                 makeTowerConstraints L (takeMiddle (map head x)) ++
                 makeTowerConstraints R (takeMiddle (map last x)),
                 (map . map) possible . takeMiddle . map takeMiddle $ x)
                
makeTowerConstraints :: Edge -> [String] -> [Constraint]
makeTowerConstraints e ls = [ tower e i (read x) | (i,x) <- zip [1..] ls, x /= ""] 

data Response = Response {
   top,bottom,left,right::String,
   cells :: [[Cell]]
} deriving (Generic, Show)
instance J.ToJSON Response where
  toEncoding = J.genericToEncoding J.defaultOptions
data Cell = Cell {
   klass :: String,
   txt :: String
} deriving (Generic, Show)
instance J.ToJSON Cell where
  toEncoding = J.genericToEncoding J.defaultOptions

toMyResponse :: [(String,LatinSquare)] -> Response -- TODO [Response] ?
toMyResponse ((comment,square):moves) = Response {
  top = "123456",
  bottom = "666666",
  left = "111222",
  right = "555666",
  cells = (map . map) toCell square
}
toCell :: [Int] -> Cell
toCell [x] = Cell { txt = show x, klass = "fixed" }
toCell xs  = Cell { txt = unwords (map show xs), klass = "possible" }
