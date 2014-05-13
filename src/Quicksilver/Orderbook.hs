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
walkBook (scanSide,accSide) o m = ((reverse reverseBook, insert leftover accSide), Accepted : (genFills fills))
  where (fills, leftover, reverseBook) = foldr (foldStep m) ([], Just o, []) scanSide
        insert Nothing accside = accside
        insert (Just o) accside = insertBag o accside
        genFills :: [(Price,Quantity)] -> Events
        genFills ((p,q):xs) = (Fill p q):(Fill p (-q)):genFills(xs)
        genFills [] = []

foldStep :: Match -> Order -> FoldCtx -> FoldCtx
foldStep m passiveOrder (fills, Nothing, book) = (fills, Nothing, passiveOrder:book)
foldStep m passiveOrder (fills, Just aggressiveOrder, book) = merge (m aggressiveOrder passiveOrder) fills book
  where merge :: MatchResult -> [(Price,Quantity)] -> [Order] -> FoldCtx
        merge (NoMatch aggressiveOrder passiveOrder) fills book = (fills, Just aggressiveOrder, passiveOrder:book)
        merge (FullMatch f@(p,quantity) Nothing) fills book = (f:fills, Nothing, book)
        merge (FullMatch f@(p,quantity) (Just o)) fills book = (f:fills, Nothing, o:book)
        merge (PartialMatch f@(p,quantity) Nothing) fills book = (f:fills, Nothing, book)
        merge (PartialMatch f@(p,quantity) r@(Just _)) fills book = (f:fills, r, book)

-- avoid bringing in a lib and just implement this for the time being.
insertBag :: (Ord a) => a -> [a] -> [a]
insertBag a [] = [a]
insertBag a (x:xs)
  | a > x = a:x:xs
  | otherwise = x:(insertBag a xs)