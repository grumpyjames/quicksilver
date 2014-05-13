module Quicksilver.Order(Order(..),
                         Quantity,
                         Price,
                         Match,
                         MatchResult(..),
                         matcher,
                         validOrder) where

type Price = Int                     
type Quantity = Int

data Order = Order Price Quantity
             deriving (Show, Eq)                 
                                            
instance Ord Order where compare = cmpOrder
                
cmpOrder (Order p1 q1) (Order p2 q2)
  | q1 > 0 = compare p1 p2
  | otherwise = compare (-p1) (-p2)

type Match = Order -> Order -> MatchResult

data MatchResult = FullMatch (Price, Quantity) (Maybe Order)
                 | PartialMatch (Price, Quantity) (Maybe Order)
                 | NoMatch Order Order
                   
fill :: Order -> Quantity -> (Maybe Order)
fill (Order p q) fillQty
  | q - fillQty /= 0 = Just $ Order p (q - fillQty)
  | otherwise = Nothing

validOrder (Order p q) = and [q /= 0, p > 0]

matcher :: Order -> Match
matcher (Order p q)
  | q > 0 = matchOrder (>=)
  | q < 0 = matchOrder (<=)
  | otherwise = undefined

matchOrder :: (Int -> Int -> Bool) -> Match
matchOrder gt o1@(Order p1 q1) o2@(Order p2 q2)
  | and[p1 `gt` p2, abs q2 > abs q1] = FullMatch (p2, q1) (fill o2 (-q1))
  | and[p1 `gt` p2, abs q1 >= abs q2] = PartialMatch (p2, (-q2)) (fill o1 (-q2))
  | otherwise = NoMatch o1 o2
  where fillQty = (signum q1) * min (abs q1) (abs q2)                  



