{-# LANGUAGE OverloadedStrings #-}
import GPolyline

import Web.Scotty
import qualified Data.Text.Lazy as T
import Data.List
import Text.Printf

import Data.Monoid (mconcat)

showfloat :: Double -> String
showfloat f = printf "%.5f" f
  
main = scotty 3000 $ do
  post "/decodegpolyline" $ do
    beam <- param "polylines"
    let res = map decodeline (lines (filter (/='\r') beam))
    let roads = map (\line -> intercalate "," ( map (\(x,y) -> "[" ++ showfloat x ++ "," ++ showfloat y ++ "]") line)) res
    let txt = intercalate "," (map (\r -> "[" ++ r ++ "]") roads)
    html $ mconcat ["[", T.pack txt, "]"]

  get "/" $ do
    html $ mconcat ["<html><head></head><body><p>Enter Google Polylines, one per line</p><form action=\"/decodegpolyline\" method=\"post\"><textarea cols=\"100\" rows=\"20\" name=\"polylines\">_p~iF~ps|U_ulLnnqC_mqNvxq`@</textarea><br /><input type=\"submit\" value=\"Decode\" /></form></body></html>"]
