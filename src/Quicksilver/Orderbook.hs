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
type Orderbook = [Order]              
              
emptyOrderbook :: Orderbook
emptyOrderbook = []

validOrder (Order p q) = and [q /= 0, p /= 0]

placeOrder :: Order -> Orderbook -> (Orderbook, Events)
placeOrder o ob 
  | validOrder o = walkBook o ob
  | otherwise = ([], [Rejected])                               
  where walkBook o [] = ([o], [Accepted])
        walkBook (Order p1 q1) [(Order p2 q2)] = ([], [Fill p1 q1, Fill p2 q2])