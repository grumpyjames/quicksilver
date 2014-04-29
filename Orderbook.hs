import Control.Monad

import Test.QuickCheck
import Test.Hspec

data PlaceResult = Accepted                   
                 | Rejected String
                 deriving (Show, Eq)
              
type Price = Int                     

type Quantity = Int

data Order = Order Price Quantity
             deriving (Show, Eq)
                      
type Orderbook = [Order]              
              
emptyOrderbook :: Orderbook
emptyOrderbook = []

placeOrder :: Order -> Orderbook -> (Orderbook, PlaceResult)
placeOrder o ob = ([o], Accepted)

instance Arbitrary Order where
  arbitrary = liftM2 Order arbitrary arbitrary

main = hspec $ do
  describe "Orderbooks" $ do
    it "always accepts an order if it is empty" $ property $ \o ->
      (snd $ placeOrder o emptyOrderbook) == Accepted
    it "consists of the only order in them after a single place" $ property $ \o ->
      (fst $ placeOrder o emptyOrderbook) == [o]
      
      