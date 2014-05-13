module Quicksilver.Orderbook (Events,
                              PlaceResult(..),
                              Price,
                              Quantity,
                              Order(..),
                              Orderbook,
                              emptyOrderbook,
                              placeOrder) where

import Quicksilver.Order

import Control.Monad


type Events = [PlaceResult]
data PlaceResult = Accepted                   
                 | Rejected
                 | Fill Price Quantity
                 deriving (Show, Eq)
              
type Orderbook = ([Order],[Order])

data Side = Bid 
          | Ask
          deriving (Show, Eq)
                   
type Reconstitute = [Order] -> [Order] -> Orderbook         
 
side :: Order -> Side
side (Order _ a)
  | a > 0 = Bid
  | otherwise = Ask            
              
emptyOrderbook :: Orderbook
emptyOrderbook = ([],[])

arrange :: Side -> Orderbook -> (Orderbook, Reconstitute)
arrange Bid (bids, asks) = ((asks, bids), \a b -> (b, a))
arrange Ask (bids, asks) = ((bids, asks), \a b -> (a, b))

placeOrder :: Order -> Orderbook -> (Orderbook, Events)
placeOrder o ob 
  | validOrder o = walkBook matchSide passiveSide o matchFn reassemble
  | otherwise = (ob, [Rejected])                               
  where ((matchSide, passiveSide), reassemble) = arrange (side o) ob
        matchFn = matcher o
        
type FoldCtx = ([(Price,Quantity)], Maybe Order, [Order])

foldStep :: Match -> Order -> FoldCtx -> FoldCtx
foldStep m passiveOrder (fills, Nothing, book) = (fills, Nothing, passiveOrder:book)
foldStep m passiveOrder (fills, Just aggressiveOrder, book) = merge (m aggressiveOrder passiveOrder) fills book
  where merge :: MatchResult -> [(Price,Quantity)] -> [Order] -> FoldCtx
        merge (NoMatch aggressiveOrder passiveOrder) fills book = (fills, Just aggressiveOrder, passiveOrder:book)
        merge (FullMatch f@(p,quantity) Nothing) fills book = (f:fills, Nothing, book)
        merge (FullMatch f@(p,quantity) (Just o)) fills book = (f:fills, Nothing, o:book)
        merge (PartialMatch f@(p,quantity) Nothing) fills book = (f:fills, Nothing, book)
        merge (PartialMatch f@(p,quantity) r@(Just _)) fills book = (f:fills, r, book)
        
walkBook :: [Order] -> [Order] -> Order -> Match -> Reconstitute -> (Orderbook, Events)
walkBook [] accSide o _ r = (r [] (insertBag o accSide), [Accepted])
walkBook scanSide accSide o m r = (r (reverse reverseBook) (insert leftover accSide), Accepted : (genFills fills))
  where (fills, leftover, reverseBook) = foldr (foldStep m) ([], Just o, []) scanSide
        insert Nothing accside = accside
        insert (Just o) accside = insertBag o accside
        genFills :: [(Price,Quantity)] -> Events
        genFills ((p,q):xs) = (Fill p q):(Fill p (-q)):genFills(xs)
        genFills [] = []

-- avoid bringing in a lib and just implement this for the time being.
insertBag :: (Ord a) => a -> [a] -> [a]
insertBag a [] = [a]
insertBag a (x:xs)
  | a > x = a:x:xs
  | otherwise = x:(insertBag a xs)

remainder :: Reconstitute -> Price -> Price -> Quantity -> Quantity -> Orderbook
remainder r p1 p2 q1 q2 
  | q1 + q2 == 0 = r [] []
  | signum (q1 + q2) == signum q1 = r [Order p1 (q1 + q2)] [] 
  | otherwise = r [] [Order p2 (q1+q2)]