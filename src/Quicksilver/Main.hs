module Quicksilver.Main (qs_main) where

import Quicksilver.Orderbook

qs_main :: IO()
qs_main = main' emptyOrderbook where 
  main' ob = do
    price <- readLn
    quantity <- readLn    
    let (newBook, results) = placeOrder (PlaceOrder price quantity) ob
    putStrLn $ show newBook ++ ", " ++ show results
    main' newBook