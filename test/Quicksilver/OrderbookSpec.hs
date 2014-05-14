module Quicksilver.OrderbookSpec (runTests,placeInstructions) where

import Quicksilver.Order
import Quicksilver.Orderbook

import Control.Monad
import qualified Data.Set

import Test.QuickCheck
import Test.Hspec

(>!>) :: (a, b) -> (a -> (a, b)) -> (a, b)
(>!>) tuple f = f $ fst tuple

instance Arbitrary Instruction where
  arbitrary = liftM2 PlaceOrder arbitrary arbitrary 

opposite :: Instruction -> Instruction
opposite (PlaceOrder p q) = PlaceOrder p (-q)

fullFillOf :: Instruction -> Event
fullFillOf (PlaceOrder p q) = Fill p q

invalidPrice :: Instruction -> Bool
invalidPrice (PlaceOrder p _) = p <= 0

validOrder :: Instruction -> Bool
validOrder (PlaceOrder p q) = and[p > 0, q /= 0]

pickSide :: Quantity -> [Instruction] -> Orderbook
pickSide q orders
  | q > 0 = (map ugh orders, [])
  | otherwise = ([], map ugh orders)
  where ugh (PlaceOrder px qy) = Order px qy

placeInstructions :: [Instruction] -> Orderbook
placeInstructions = foldr step emptyOrderbook
  where step :: Instruction -> Orderbook -> Orderbook
        step o = fst . placeOrder o
        
price :: Instruction -> Int
price (PlaceOrder p _) = p

runTests :: IO()
runTests = hspec $ do
  describe "Orderbooks" $ do
    it "rejects orders with zero quantity" $ do
      placeOrder (PlaceOrder 23 0) emptyOrderbook `shouldBe` (([],[]), [Rejected])
    it "rejects orders with invalid price" $ property $ \o ->
      (invalidPrice o) ==> placeOrder o emptyOrderbook `shouldBe` (([],[]), [Rejected])
    it "always accepts valid orders if it is empty" $ property $ \o ->
      (validOrder o) ==> (snd $ placeOrder o emptyOrderbook) == [Accepted]
    it "consists of the only order in them after a single place" $ property $ \o@(PlaceOrder p q) ->
      and [p > 0, q /= 0] ==> (fst $ placeOrder o emptyOrderbook) == pickSide q [o]
    it "fills both sides of equal and opposite orders when placed consecutively" $ property $ \o@(PlaceOrder p q)  ->
      and[p > 0, q /= 0] ==> placeOrder o emptyOrderbook >!> (placeOrder $ opposite o) == (([],[]), [Accepted, fullFillOf $ opposite o, fullFillOf o])
    it "accepts the same order twice, leaving it on the book" $ property $ \o@(PlaceOrder _ q) ->
      (validOrder o) ==> placeOrder o emptyOrderbook >!> placeOrder o == (pickSide q [o,o], [Accepted])
    it "places opposite orders that don't match on opposite sides of the book" $ property $ \(PlaceOrder p q) ->
      and[p > 0, q > 0] ==> placeOrder (PlaceOrder p q) emptyOrderbook >!> placeOrder (PlaceOrder (p+1) (-q)) == (([Order p q],[Order (p+1) (-q)]), [Accepted])
    it "matches a buy order with a sell order with a lower price, the aggressing order taking the best price" $ property $ \(PlaceOrder p q) ->
      and[p > 1, q > 0] ==> placeOrder (PlaceOrder p q) emptyOrderbook >!> placeOrder (PlaceOrder (p-1) (-q)) == (([],[]), [Accepted, Fill p (-q), Fill p q])
    it "matches only half of a double reversed order, leaving the remainder on the book" $ property $ \(PlaceOrder p q) ->
      and[p > 0, q /= 0] ==> placeOrder (PlaceOrder p q) emptyOrderbook >!> placeOrder (PlaceOrder p ((-2)*q)) == (pickSide (-q) [PlaceOrder p (-q)], [Accepted, Fill p (-q), Fill p q])
    it "has the same representation regardless of the entry order of two orders with different prices" $ property $ \(o1@(PlaceOrder p _),o2@(PlaceOrder r _)) ->
      p /= r ==> placeInstructions [o1, o2] == placeInstructions [o2, o1]
    it "can match two orders on the opposite side" $ property $ \q ->
      q /= 0 ==> placeOrder (PlaceOrder 1 ((-3)*q)) (placeInstructions [PlaceOrder 1 q, PlaceOrder 1 (2*q)]) == (([],[]), [Accepted, Fill 1 (-q), Fill 1 q, Fill 1 ((-2)*q), Fill 1 (2*q)])
    it "has the same representation regardless of the entry order of n orders with different prices" $ property $ \os ->
      Data.Set.size (Data.Set.fromList (map price os)) == length os ==> (placeInstructions os) == (placeInstructions $ reverse os)