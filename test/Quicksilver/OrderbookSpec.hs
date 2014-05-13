module Quicksilver.OrderbookSpec (runTests,placeOrders) where

import Quicksilver.Order
import Quicksilver.Orderbook

import Control.Monad

import Test.QuickCheck
import Test.Hspec

(>!>) :: (a, b) -> (a -> (a, b)) -> (a, b)
(>!>) tuple f = f $ fst tuple

instance Arbitrary Order where
  arbitrary = liftM2 Order arbitrary arbitrary 

opposite :: Order -> Order
opposite (Order p q) = Order p (-q)

fullFillOf :: Order -> Event
fullFillOf (Order p q) = Fill p q

invalidPrice :: Order -> Bool
invalidPrice (Order p _) = p <= 0

pickSide :: Quantity -> [Order] -> Orderbook
pickSide q orders
  | q > 0 = (orders, [])
  | otherwise = ([], orders)

placeOrders :: [Order] -> Orderbook
placeOrders = foldr step emptyOrderbook
  where step :: Order -> Orderbook -> Orderbook
        step o = fst . placeOrder o

runTests :: IO()
runTests = hspec $ do
  describe "Orderbooks" $ do
    it "rejects orders with zero quantity" $ do
      placeOrder (Order 23 0) emptyOrderbook `shouldBe` (([],[]), [Rejected])
    it "rejects orders with invalid price" $ property $ \o ->
      (invalidPrice o) ==> placeOrder o emptyOrderbook `shouldBe` (([],[]), [Rejected])
    it "always accepts valid orders if it is empty" $ property $ \o ->
      (validOrder o) ==> (snd $ placeOrder o emptyOrderbook) == [Accepted]
    it "consists of the only order in them after a single place" $ property $ \o@(Order p q) ->
      and [p > 0, q /= 0] ==> (fst $ placeOrder o emptyOrderbook) == pickSide q [o]
    it "fills both sides of equal and opposite orders when placed consecutively" $ property $ \o@(Order p q)  ->
      and[p > 0, q /= 0] ==> placeOrder o emptyOrderbook >!> (placeOrder $ opposite o) == (([],[]), [Accepted, fullFillOf $ opposite o, fullFillOf o])
    it "accepts the same order twice, leaving it on the book" $ property $ \o@(Order _ q) ->
      (validOrder o) ==> placeOrder o emptyOrderbook >!> placeOrder o == (pickSide q [o,o], [Accepted])
    it "places opposite orders that don't match on opposite sides of the book" $ property $ \(Order p q) ->
      and[p > 0, q > 0] ==> placeOrder (Order p q) emptyOrderbook >!> placeOrder (Order (p+1) (-q)) == (([Order p q],[Order (p+1) (-q)]), [Accepted])
    it "matches a buy order with a sell order with a lower price, the aggressing order taking the best price" $ property $ \(Order p q) ->
      and[p > 1, q > 0] ==> placeOrder (Order p q) emptyOrderbook >!> placeOrder (Order (p-1) (-q)) == (([],[]), [Accepted, Fill p (-q), Fill p q])
    it "matches only half of a double reversed order, leaving the remainder on the book" $ property $ \(Order p q) ->
      and[p > 0, q /= 0] ==> placeOrder (Order p q) emptyOrderbook >!> placeOrder (Order p ((-2)*q)) == (pickSide (-q) [Order p (-q)], [Accepted, Fill p (-q), Fill p q])
    it "has the same representation regardless of the entry order of two orders with different prices" $ property $ \(o1@(Order p _),o2@(Order r _)) ->
      p /= r ==> placeOrders [o1, o2] == placeOrders [o2, o1]
    it "can match two orders on the opposite side" $ property $ \q ->
      q /= 0 ==> placeOrder (Order 1 ((-3)*q)) (placeOrders [Order 1 q, Order 1 (2*q)]) == (([],[]), [Accepted, Fill 1 ((-2)*q), Fill 1 (2*q), Fill 1 (-q), Fill 1 q])