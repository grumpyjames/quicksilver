module Quicksilver.Orderbook (Event(..),
                              Events,
                              Price,
                              Quantity,
                              Order(..),
                              Orderbook,
                              emptyOrderbook,
                              placeOrder) where

import Quicksilver.Order

import Control.Monad
import Data.Tuple

type Events = [Event]
data Event = Accepted                   
           | Rejected
           | Fill Price Quantity
           deriving (Show, Eq)
              
type Orderbook = ([Order],[Order])

data Side = Bid 
          | Ask
          deriving (Show, Eq)
                   
type Reconstitute = Orderbook -> Orderbook         
 
side :: Order -> Side
side (Order _ a)
  | a > 0 = Bid
  | otherwise = Ask            
              
emptyOrderbook :: Orderbook
emptyOrderbook = ([],[])

arrange :: Side -> Reconstitute
arrange Bid = swap
arrange Ask = id

placeOrder :: Order -> Orderbook -> (Orderbook, Events)
placeOrder o ob 
  | validOrder o = (r newBook, events)
  | otherwise = (ob, [Rejected])                               
  where r = arrange (side o)
        matchFn = matcher o
        (newBook,events) = walkBook (r ob) o matchFn
        
type FoldCtx = ([(Price,Quantity)], Maybe Order, [Order])
       
walkBook :: Orderbook -> Order -> Match -> (Orderbook, Events)
walkBook (scanSide,accSide) o m = ((newBook, insert leftover accSide), Accepted : (genFills fills))
  where (fills, leftover, newBook) = walkBook' m scanSide o []
        insert Nothing accside = accside
        insert (Just o) accside = insertBag o accside
        genFills :: [(Price,Quantity)] -> Events
        genFills ((p,q):xs) = (Fill p q):(Fill p (-q)):genFills(xs)
        genFills [] = []
        
walkBook' :: Match -> [Order] -> Order -> [(Price,Quantity)] -> FoldCtx
walkBook' m [] o fills = (fills, Just o, [])  
walkBook' m (x:xs) o fills = cin m (m o x) xs fills
  where cin :: Match -> MatchResult -> [Order] -> [(Price,Quantity)] -> FoldCtx
        cin m (PartialMatch f@(p,q) (Just o)) os fs = walkBook' m os o (f:fs)
        cin m (PartialMatch f@(p,q) Nothing) os fs = (f:fs, Nothing, os)
        cin m (FullMatch f@(p,q) (Just o)) os fs = (f:fs, Nothing, o:os)
        cin m (FullMatch f@(p,q) Nothing) os fs = (f:fs, Nothing, os)
        cin m (NoMatch agg pass) os fs = ([], Just agg, pass:os)

-- avoid bringing in a lib and just implement this for the time being.
insertBag :: (Ord a) => a -> [a] -> [a]
insertBag a [] = [a]
insertBag a (x:xs)
  | a > x = a:x:xs
  | otherwise = x:(insertBag a xs)