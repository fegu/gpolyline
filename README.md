gpolyline
=========

Pure Haskell module for encoding/decoding Google Polylines

GPolyline.hs exports

```haskell
encodeline :: [(Double,Double)] -> String
decodeline :: String -> [(Double,Double)]
encodeunsigned :: Int -> String
decodeunsigned :: String -> Int
```

The pair of doubles are Latitude and Longitude. You will mainly use encodeline and decodeline, but the functions for unsigned can be useful for single numbers such as zoom level.

polydecode.hs is a simple command line application using GPolyline.hs for decoding.  
polyws.hs is a webservice (including a html interface for testing) using scotty.
