module Quicksilver.Orderbook (Events,
                              PlaceResult(..),
                              Price,
                              Quantity,
                              Order(..),
                              Orderbook,
                              emptyOrderbook,
                              placeOrder) where

import Control.Monad

type Events = [PlaceResult]
data PlaceResult = Accepted                   
                 | Rejected
                 | Fill Price Quantity
                 deriving (Show, Eq)
              
type Price = Int                     
type Quantity = Int
data Order = Order Price Quantity
             deriving (Show, Eq)                     
type Orderbook = ([Order],[Order])

data Side = Bid 
          | Ask
          deriving (Show, Eq)

side :: Order -> Side
side (Order _ a)
  | a > 0 = Bid
  | otherwise = Ask            
              
emptyOrderbook :: Orderbook
emptyOrderbook = ([],[])

opposite :: Side -> Side
opposite Bid = Ask
opposite Ask = Bid

sideToScan :: Side -> Orderbook -> [Order]
sideToScan Bid (_, a) = a
sideToScan Ask (a, _) = a

sideToAcc :: Side -> Orderbook -> [Order]
sideToAcc = sideToScan . opposite

validOrder (Order p q) = and [q /= 0, p > 0]

type Reconstitute = [Order] -> [Order] -> Orderbook         

rebuild :: Side -> Reconstitute
rebuild Bid a b = (b, a)
rebuild Ask a b = (a, b)

placeOrder :: Order -> Orderbook -> (Orderbook, Events)
placeOrder o ob 
  | validOrder o = walkBook scanSide accSide o reassemble
  | otherwise = (([],[]), [Rejected])                               
  where scanSide = sideToScan orderSide ob
        accSide = sideToAcc orderSide ob
        reassemble = rebuild orderSide
        orderSide = side o
        
walkBook :: [Order] -> [Order] -> Order -> Reconstitute -> (Orderbook, Events)
walkBook [] accSide o r = (r [] (o:accSide), [Accepted])
walkBook ((Order p1 q1):tail) accSide (Order p2 q2) r 
  | p1 == p2 = (r tail [], [Fill p1 (-q1), Fill p1 q1])
  | otherwise = (r [Order p1 q1] [Order p2 q2], [Accepted])
