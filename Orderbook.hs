import Control.Monad

import Test.QuickCheck
import Test.Hspec

type Events = [PlaceResult]

data PlaceResult = Accepted                   
                 | Rejected String
                 | Fill Price Quantity
                 deriving (Show, Eq)
              
type Price = Int                     

type Quantity = Int

data Order = Order Price Quantity
             deriving (Show, Eq)
                      
type Orderbook = [Order]              
              
emptyOrderbook :: Orderbook
emptyOrderbook = []

placeOrder :: Order -> Orderbook -> (Orderbook, Events)
placeOrder o ob = ([o], [Accepted])

(>!>) :: (Orderbook, Events) -> (Orderbook -> (Orderbook, Events)) -> (Orderbook, Events)
(>!>) tuple f = f $ fst tuple

instance Arbitrary Order where
  arbitrary = liftM2 Order arbitrary arbitrary

opposite :: Order -> Order
opposite (Order p q) = Order p (-q)

fullFillOf :: Order -> PlaceResult
fullFillOf (Order p q) = Fill p q

main = hspec $ do
  describe "Orderbooks" $ do
    it "always accepts an order if it is empty" $ property $ \o ->
      (snd $ placeOrder o emptyOrderbook) == [Accepted]
    it "consists of the only order in them after a single place" $ property $ \o ->
      (fst $ placeOrder o emptyOrderbook) == [o]
    it "fills both sides of equal and opposite orders when placed consecutively" $ property $ \o ->
      placeOrder o emptyOrderbook >!> (placeOrder $ opposite o) == ([], [fullFillOf o, fullFillOf $ opposite o])
      
      