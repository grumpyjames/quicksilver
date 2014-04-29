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

placeOrder :: Orderbook -> Order -> PlaceResult
placeOrder ob o = Accepted

instance Arbitrary Order where
  arbitrary = liftM2 Order arbitrary arbitrary

main = hspec $ do
  describe "Placing orders in an orderbook" $ do
    it "always accepts an order if it is empty" $ property $ \order ->
      placeOrder emptyOrderbook order == Accepted