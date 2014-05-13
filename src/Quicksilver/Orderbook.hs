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
                   
type Reconstitute = [Order] -> [Order] -> Orderbook         
type Match = Order -> Order -> MatchResult

data MatchResult = MatchSuccess (Price, Quantity) (Maybe Order) (Maybe Order)
                 | MatchFail Order Order

  
side :: Order -> Side
side (Order _ a)
  | a > 0 = Bid
  | otherwise = Ask            
              
emptyOrderbook :: Orderbook
emptyOrderbook = ([],[])

opposite :: Side -> Side
opposite Bid = Ask
opposite Ask = Bid

matchSide :: Side -> Orderbook -> [Order]
matchSide Bid (_, a) = a
matchSide Ask (a, _) = a

passiveSide :: Side -> Orderbook -> [Order]
passiveSide = matchSide . opposite

matches :: Side -> Match
matches Bid = m (>=)
matches Ask = m (<=)

m :: (Int -> Int -> Bool) -> Match
m gt o1@(Order p1 q1) o2@(Order p2 q2)
  | p1 `gt` p2 = MatchSuccess (p2, fillQty) (remaining o1 fillQty) (remaining o2 (-fillQty))
  | otherwise = MatchFail o1 o2
  where fillQty = (signum q1) * min (abs q1) (abs q2)                  
          
remaining :: Order -> Quantity -> (Maybe Order)
remaining (Order p q) fillQty
  | q - fillQty /= 0 = Just $ Order p (q - fillQty)
  | otherwise = Nothing

validOrder (Order p q) = and [q /= 0, p > 0]

rebuild :: Side -> Reconstitute
rebuild Bid a b = (b, a)
rebuild Ask a b = (a, b)

placeOrder :: Order -> Orderbook -> (Orderbook, Events)
placeOrder o ob 
  | validOrder o = walkBook match passive o matchFn reassemble
  | otherwise = (ob, [Rejected])                               
  where match = matchSide orderSide ob
        passive = passiveSide orderSide ob
        reassemble = rebuild orderSide
        matchFn = matches orderSide
        orderSide = side o        
        
type FoldCtx = ([(Price,Quantity)], Maybe Order, [Order])

foldStep :: Match -> Order -> FoldCtx -> FoldCtx
foldStep m passiveOrder (fills, Nothing, book) = (fills, Nothing, passiveOrder:book)
foldStep m passiveOrder (fills, Just aggressiveOrder, book) = merge (m aggressiveOrder passiveOrder) fills book
  where merge :: MatchResult -> [(Price,Quantity)] -> [Order] -> FoldCtx
        merge (MatchFail aggressiveOrder passiveOrder) fills book = (fills, Just aggressiveOrder, passiveOrder:book)
        merge (MatchSuccess f@(p,quantity) Nothing Nothing) fills book = (f:fills, Nothing, book)
        merge (MatchSuccess f@(p,quantity) r@(Just _) Nothing) fills book = (f:fills, r, book)
        merge (MatchSuccess f@(p,quantity) Nothing (Just o)) fills book = (f:fills, Nothing, o:book)
        merge (MatchSuccess f@(p,quantity) _ _) _ _ = undefined -- FIXME: restrict MatchResult to prevent this
        
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

instance Ord Order where compare = cmpOrder
                
cmpOrder (Order p1 q1) (Order p2 q2)
  | q1 > 0 = compare p1 p2
  | otherwise = compare (-p1) (-p2)

remainder :: Reconstitute -> Price -> Price -> Quantity -> Quantity -> Orderbook
remainder r p1 p2 q1 q2 
  | q1 + q2 == 0 = r [] []
  | signum (q1 + q2) == signum q1 = r [Order p1 (q1 + q2)] [] 
  | otherwise = r [] [Order p2 (q1+q2)]