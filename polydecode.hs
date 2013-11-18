import GPolyline (decodeline)

import Text.Printf (printf)
import Data.List (intercalate)

main :: IO ()
main = interact (package . map processone . lines . removecr)
  where removecr = filter (/='\r')
  
processone :: String -> String
processone = package . road
  where road = map (package . map showfloat . listify) . decodeline
        listify (x,y) = [x,y]
        showfloat = printf "%.5f"

package :: [String] -> String
package xs  = "[" ++ (intercalate "," xs) ++ "]"
