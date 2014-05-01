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

matches :: Side -> Match
matches Bid = (>=)
matches Ask = (<=)

validOrder (Order p q) = and [q /= 0, p > 0]

type Reconstitute = [Order] -> [Order] -> Orderbook         
type Match = Price -> Price -> Bool

rebuild :: Side -> Reconstitute
rebuild Bid a b = (b, a)
rebuild Ask a b = (a, b)

placeOrder :: Order -> Orderbook -> (Orderbook, Events)
placeOrder o ob 
  | validOrder o = walkBook scanSide accSide o match reassemble
  | otherwise = (([],[]), [Rejected])                               
  where scanSide = sideToScan orderSide ob
        accSide = sideToAcc orderSide ob
        reassemble = rebuild orderSide
        match = matches orderSide
        orderSide = side o        
        
walkBook :: [Order] -> [Order] -> Order -> Match -> Reconstitute -> (Orderbook, Events)
walkBook [] accSide o _ r = (r [] (o:accSide), [Accepted])
walkBook ((Order p1 q1):[]) accSide (Order p2 q2) m r 
  | m p2 p1 = (r [] (remainder p2 q1 q2), [Fill p1 (-q1), Fill p1 q1])
  | otherwise = (r [Order p1 q1] [Order p2 q2], [Accepted])
  where remainder p q1 q2 
          | q1 + q2 == 0 = []
          | otherwise = [Order p (q1+q2)]