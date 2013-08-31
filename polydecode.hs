import GPolyline

import Text.Printf
import System.Environment
import Data.List
import System.IO

showfloat :: Double -> String
showfloat f = printf "%.5f" f
  
main = do
  beam <- getLine
  let res = map decodeline (lines (filter (/='\r') beam))
  let roads = map (\line -> intercalate "," ( map (\(x,y) -> "[" ++ showfloat x ++ "," ++ showfloat y ++ "]") line)) res
  let txt = intercalate "," (map (\r -> "[" ++ r ++ "]") roads)
  putStr $ "[" ++ txt ++ "]"
