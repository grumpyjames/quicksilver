module Quicksilver.Order(Order(..),
                         order,
                         Quantity,
                         Price,
                         Match,
                         MatchResult(..),
                         matcher) where

type Price = Int                     
type Quantity = Int
data Order = Order Price Quantity
             deriving (Show, Eq)                 
                                            
instance Ord Order where compare = cmpOrder
                
order :: Int -> Int -> Maybe Order
order p q
  | and[p > 0, q /= 0] = Just $ Order p q
  | otherwise = Nothing  

cmpOrder (Order p1 q1) (Order p2 q2)
  | q1 > 0 = compare p1 p2
  | otherwise = compare (-p1) (-p2)

type Match = Order -> Order -> MatchResult

data MatchResult = BothComplete (Price, Quantity)
                 | AggressiveComplete (Price, Quantity) Order
                 | PassiveComplete (Price, Quantity) Order
                 | NoMatch Order Order
                   
fill :: Order -> Quantity -> (Maybe Order)
fill (Order p q) fillQty
  | q - fillQty /= 0 = Just $ Order p (q - fillQty)
  | otherwise = Nothing

matcher :: Order -> Match
matcher (Order p q)
  | q > 0 = matchOrder (>=)
  | q < 0 = matchOrder (<=)
  | otherwise = undefined

matchOrder :: (Int -> Int -> Bool) -> Match
matchOrder gt o1@(Order p1 q1) o2@(Order p2 q2)
  | and[p1 `gt` p2, abs q1 == abs q2] = BothComplete (p2, q1)
  | and[p1 `gt` p2, abs q2 > abs q1] = AggressiveComplete (p2, q1) (Order p2 (q1+q2))
  | and[p1 `gt` p2, abs q1 > abs q2] = PassiveComplete (p2, (-q2)) (Order p1 (q1+q2))
  | otherwise = NoMatch o1 o2


