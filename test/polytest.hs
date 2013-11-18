import GPolyline

-- Simple test comparing the decoding of a polyline (string) to known good output
-- Max error must be below the precision of the polyline format, which is 5 decimal places

import Data.List.Split (chunksOf)

main = do
    target' <- target
    ln <- polyline
    let poi = decodeline ln
    let target = map tupleup (read target')
    let (x2,y2) = unzip target
    let (x3,y3) = unzip poi
    let xdiff = map abs $ zipWith (-) x2 x3
    let ydiff = map abs $ zipWith (-) y2 y3
    putStrLn $ "Max X error on decode <1e-5 " ++ if maximum xdiff < 1e-5 then "OK" else "ERROR"
    putStrLn $ "Max Y error on decode <1e-5 " ++ if maximum ydiff < 1e-5 then "OK" else "ERROR"
    let tegrat = encodeline poi
    putStrLn "Encoded line:                  Known-good:                    Equal:"
    let m = map (\(a,b) -> b ++ " " ++ a ++ (if a==b then " OK" else " ERROR")) (zip (chunksOf 30 tegrat) (chunksOf 30 ln))
    mapM_ putStrLn m


tupleup :: [Double] -> (Double,Double)
tupleup (x:y:_) = (x,y)

polyline    = readFile "polyline_test.txt"
target      = readFile "polyline_decoded.txt"
