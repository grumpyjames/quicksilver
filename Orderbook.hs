import Control.Monad

import Test.QuickCheck
import Test.Hspec

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

(>!>) :: (Orderbook, Events) -> (Orderbook -> (Orderbook, Events)) -> (Orderbook, Events)
(>!>) tuple f = f $ fst tuple

instance Arbitrary Order where
  arbitrary = liftM2 Order arbitrary arbitrary

opposite :: Order -> Order
opposite (Order p q) = Order p (-q)

fullFillOf :: Order -> PlaceResult
fullFillOf (Order p q) = Fill p q

quantityZero :: Order -> Bool
quantityZero (Order p q) = (q == 0)

main = hspec $ do
  describe "Orderbooks" $ do
    it "rejects orders with zero quantity" $ do
      placeOrder (Order 23 0) emptyOrderbook `shouldBe` ([], [Rejected])
    it "rejects orders with zero price" $ do
      placeOrder (Order 0 23) emptyOrderbook `shouldBe` ([], [Rejected])
    it "always accepts valid orders if it is empty" $ property $ \o ->
      (validOrder o) ==> (snd $ placeOrder o emptyOrderbook) == [Accepted]
    it "consists of the only order in them after a single place" $ property $ \o ->
      (validOrder o) ==> (fst $ placeOrder o emptyOrderbook) == [o]
    it "fills both sides of equal and opposite orders when placed consecutively" $ property $ \o ->
      (validOrder o) ==> placeOrder o emptyOrderbook >!> (placeOrder $ opposite o) == ([], [fullFillOf o, fullFillOf $ opposite o])
      
      