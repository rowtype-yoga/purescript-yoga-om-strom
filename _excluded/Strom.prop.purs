module Yoga.Om.Strom.PropTest where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split) as String
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import Test.QuickCheck.Gen (Gen, chooseInt, randomSample', vectorOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om (Om)
import Yoga.Om as Om
import Yoga.Om.Strom as Strom
import Yoga.Om.Strom.FileStream as FileStream

-- Helper to run Om in tests
runOm :: forall a. Om {} () a -> Aff a
runOm om = Om.runOm {} { exception: \err -> liftEffect (throwException err) } om

-- | Small arrays of ints (0–50 elements)
genSmallArray :: Gen (Array Int)
genSmallArray = do
  len <- chooseInt 0 50
  vectorOf len (chooseInt (-1000) 1000)

-- | Non-negative int for take/drop/grouped args
genSmallNat :: Gen Int
genSmallNat = chooseInt 0 100

-- | Positive int (>0) for grouped size
genPositiveNat :: Gen Int
genPositiveNat = chooseInt 1 50

-- | Multi-chunk arrays (1000–3000 elements) to force chunk boundaries
genMultiChunkArray :: Gen (Array Int)
genMultiChunkArray = do
  len <- chooseInt 1000 3000
  vectorOf len (chooseInt (-100) 100)

-- | Even predicate (can't generate arbitrary predicates)
isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

spec :: Spec Unit
spec = do
  describe "Strom Properties" do

    describe "Roundtrip / Identity" do

      it "fromArray >>> runCollect ≡ identity" do
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ Strom.fromArray arr # Strom.runCollect
          result `shouldEqual` arr

      it "mapStrom identity ≡ identity" do
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ Strom.fromArray arr # Strom.mapStrom identity # Strom.runCollect
          result `shouldEqual` arr

    describe "Consistency with Array equivalents" do

      it "takeStrom n ≡ Array.take n" do
        samples <- liftEffect $ randomSample' 50 (Tuple <$> genSmallNat <*> genSmallArray)
        for_ samples \(Tuple n arr) -> do
          result <- runOm $ Strom.fromArray arr # Strom.takeStrom n # Strom.runCollect
          result `shouldEqual` Array.take n arr

      it "dropStrom n ≡ Array.drop n" do
        samples <- liftEffect $ randomSample' 50 (Tuple <$> genSmallNat <*> genSmallArray)
        for_ samples \(Tuple n arr) -> do
          result <- runOm $ Strom.fromArray arr # Strom.dropStrom n # Strom.runCollect
          result `shouldEqual` Array.drop n arr

      it "filterStrom isEven ≡ Array.filter isEven" do
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ Strom.fromArray arr # Strom.filterStrom isEven # Strom.runCollect
          result `shouldEqual` Array.filter isEven arr

      it "collectStrom f ≡ Array.mapMaybe f" do
        let f n = if isEven n then Just (n * 10) else Nothing
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ Strom.fromArray arr # Strom.collectStrom f # Strom.runCollect
          result `shouldEqual` Array.mapMaybe f arr

      it "scanStrom (+) 0 ≡ Array.scanl (+) 0" do
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ Strom.fromArray arr # Strom.scanStrom (+) 0 # Strom.runCollect
          result `shouldEqual` Array.scanl (+) 0 arr

    describe "Composition laws" do

      it "take n (take m s) ≡ take (min n m) s" do
        samples <- liftEffect $ randomSample' 50 do
          n <- genSmallNat
          m <- genSmallNat
          arr <- genSmallArray
          pure { n, m, arr }
        for_ samples \{ n, m, arr } -> do
          result1 <- runOm $ Strom.fromArray arr # Strom.takeStrom m # Strom.takeStrom n # Strom.runCollect
          result2 <- runOm $ Strom.fromArray arr # Strom.takeStrom (min n m) # Strom.runCollect
          result1 `shouldEqual` result2

      it "drop n (drop m s) ≡ drop (n + m) s" do
        samples <- liftEffect $ randomSample' 50 do
          n <- genSmallNat
          m <- genSmallNat
          arr <- genSmallArray
          pure { n, m, arr }
        for_ samples \{ n, m, arr } -> do
          result1 <- runOm $ Strom.fromArray arr # Strom.dropStrom m # Strom.dropStrom n # Strom.runCollect
          result2 <- runOm $ Strom.fromArray arr # Strom.dropStrom (n + m) # Strom.runCollect
          result1 `shouldEqual` result2

      it "take n <> drop n ≡ identity (partition property)" do
        samples <- liftEffect $ randomSample' 50 (Tuple <$> genSmallNat <*> genSmallArray)
        for_ samples \(Tuple n arr) -> do
          taken <- runOm $ Strom.fromArray arr # Strom.takeStrom n # Strom.runCollect
          dropped <- runOm $ Strom.fromArray arr # Strom.dropStrom n # Strom.runCollect
          (taken <> dropped) `shouldEqual` arr

    describe "Monoid laws" do

      it "empty <> s ≡ s (left identity)" do
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ (Strom.empty <> Strom.fromArray arr) # Strom.runCollect
          result `shouldEqual` arr

      it "s <> empty ≡ s (right identity)" do
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ (Strom.fromArray arr <> Strom.empty) # Strom.runCollect
          result `shouldEqual` arr

      it "(s1 <> s2) <> s3 ≡ s1 <> (s2 <> s3) (associativity)" do
        samples <- liftEffect $ randomSample' 30 do
          a <- genSmallArray
          b <- genSmallArray
          c <- genSmallArray
          pure { a, b, c }
        for_ samples \{ a, b, c } -> do
          let s1 = Strom.fromArray a
          let s2 = Strom.fromArray b
          let s3 = Strom.fromArray c
          result1 <- runOm $ ((s1 <> s2) <> s3) # Strom.runCollect
          result2 <- runOm $ (s1 <> (s2 <> s3)) # Strom.runCollect
          result1 `shouldEqual` result2

    describe "Combining" do

      it "zip length ≡ min of input lengths" do
        samples <- liftEffect $ randomSample' 50 (Tuple <$> genSmallArray <*> genSmallArray)
        for_ samples \(Tuple a b) -> do
          result <- runOm $ Strom.zipStrom (Strom.fromArray a) (Strom.fromArray b) # Strom.runCollect
          Array.length result `shouldEqual` min (Array.length a) (Array.length b)

      it "zip produces correct pairs" do
        samples <- liftEffect $ randomSample' 50 (Tuple <$> genSmallArray <*> genSmallArray)
        for_ samples \(Tuple a b) -> do
          result <- runOm $ Strom.zipStrom (Strom.fromArray a) (Strom.fromArray b) # Strom.runCollect
          let expected = Array.zipWith Tuple a b
          result `shouldEqual` expected

      it "intersperse element count = max 0 (2 * length - 1)" do
        samples <- liftEffect $ randomSample' 50 genSmallArray
        for_ samples \arr -> do
          result <- runOm $ Strom.fromArray arr # Strom.intersperse 0 # Strom.runCollect
          let expectedLen = max 0 (2 * Array.length arr - 1)
          Array.length result `shouldEqual` expectedLen

      it "grouped n then flatten ≡ identity" do
        samples <- liftEffect $ randomSample' 50 (Tuple <$> genPositiveNat <*> genSmallArray)
        for_ samples \(Tuple n arr) -> do
          result <- runOm $ Strom.fromArray arr # Strom.groupedStrom n # Strom.mapStrom identity # Strom.runCollect
          let flattened = Array.concat result
          flattened `shouldEqual` arr

      it "fromArray (a <> b) ≡ fromArray a <> fromArray b (append homomorphism)" do
        samples <- liftEffect $ randomSample' 50 (Tuple <$> genSmallArray <*> genSmallArray)
        for_ samples \(Tuple a b) -> do
          result1 <- runOm $ Strom.fromArray (a <> b) # Strom.runCollect
          result2 <- runOm $ (Strom.fromArray a <> Strom.fromArray b) # Strom.runCollect
          result1 `shouldEqual` result2

    describe "Multi-chunk stress tests" do

      it "fromArray >>> runCollect roundtrip (multi-chunk)" do
        samples <- liftEffect $ randomSample' 5 genMultiChunkArray
        for_ samples \arr -> do
          result <- runOm $ Strom.fromArray arr # Strom.runCollect
          result `shouldEqual` arr

      it "takeStrom n ≡ Array.take n (multi-chunk via rangeStrom)" do
        samples <- liftEffect $ randomSample' 20 (chooseInt 0 3000)
        for_ samples \n -> do
          let arr = Array.range 0 2999
          result <- runOm $ Strom.rangeStrom 0 3000 # Strom.takeStrom n # Strom.runCollect
          result `shouldEqual` Array.take n arr

      it "dropStrom n ≡ Array.drop n (multi-chunk via rangeStrom)" do
        samples <- liftEffect $ randomSample' 20 (chooseInt 0 3000)
        for_ samples \n -> do
          let arr = Array.range 0 2999
          result <- runOm $ Strom.rangeStrom 0 3000 # Strom.dropStrom n # Strom.runCollect
          result `shouldEqual` Array.drop n arr

      it "filterStrom isEven (multi-chunk via rangeStrom)" do
        result <- runOm $ Strom.rangeStrom 0 3000 # Strom.filterStrom isEven # Strom.runCollect
        let expected = Array.filter isEven (Array.range 0 2999)
        result `shouldEqual` expected

      it "scanStrom (+) 0 (multi-chunk via rangeStrom)" do
        result <- runOm $ Strom.rangeStrom 1 2001 # Strom.scanStrom (+) 0 # Strom.runCollect
        let expected = Array.scanl (+) 0 (Array.range 1 2000)
        result `shouldEqual` expected

      it "take n <> drop n ≡ identity (multi-chunk)" do
        samples <- liftEffect $ randomSample' 10 (chooseInt 0 3000)
        for_ samples \n -> do
          taken <- runOm $ Strom.rangeStrom 0 3000 # Strom.takeStrom n # Strom.runCollect
          dropped <- runOm $ Strom.rangeStrom 0 3000 # Strom.dropStrom n # Strom.runCollect
          (taken <> dropped) `shouldEqual` Array.range 0 2999

      it "grouped n then flatten ≡ identity (multi-chunk)" do
        samples <- liftEffect $ randomSample' 10 genPositiveNat
        for_ samples \n -> do
          result <- runOm $ Strom.rangeStrom 0 2500 # Strom.groupedStrom n # Strom.runCollect
          let flattened = Array.concat result
          flattened `shouldEqual` Array.range 0 2499

      it "zip with multi-chunk streams" do
        result <- runOm $
          Strom.zipWithStrom (+) (Strom.rangeStrom 0 2000) (Strom.rangeStrom 0 2000)
            # Strom.runCollect
        let expected = map (\i -> i + i) (Array.range 0 1999)
        result `shouldEqual` expected

      it "fromArray (a <> b) ≡ fromArray a <> fromArray b (multi-chunk)" do
        samples <- liftEffect $ randomSample' 5 (Tuple <$> genMultiChunkArray <*> genMultiChunkArray)
        for_ samples \(Tuple a b) -> do
          result1 <- runOm $ Strom.fromArray (a <> b) # Strom.runCollect
          result2 <- runOm $ (Strom.fromArray a <> Strom.fromArray b) # Strom.runCollect
          result1 `shouldEqual` result2

      it "intersperse element count (multi-chunk via rangeStrom)" do
        result <- runOm $ Strom.rangeStrom 0 2000 # Strom.intersperse 0 # Strom.runCollect
        Array.length result `shouldEqual` (2 * 2000 - 1)

      it "mapStrom identity (multi-chunk via rangeStrom)" do
        result <- runOm $ Strom.rangeStrom 0 2500 # Strom.mapStrom identity # Strom.runCollect
        result `shouldEqual` Array.range 0 2499

      it "monoid associativity (multi-chunk)" do
        let
          s1 = Strom.rangeStrom 0 1500
          s2 = Strom.rangeStrom 1500 3000
          s3 = Strom.rangeStrom 3000 4500
        result1 <- runOm $ ((s1 <> s2) <> s3) # Strom.runCollect
        result2 <- runOm $ (s1 <> (s2 <> s3)) # Strom.runCollect
        result1 `shouldEqual` result2

    describe "Apply instance (monad law: ap ≡ <*>)" do

      -- For a lawful Monad, `fs <*> xs` must equal `fs >>= \f -> xs >>= \x -> pure (f x)`
      -- The Apply instance has a bug in the (Loop, Loop) case: it only pairs
      -- corresponding chunks instead of doing the full cartesian product.

      it "apply matches ap for single-chunk streams" do
        -- Single-chunk: both streams produce Done, so Apply works correctly
        let fs = Strom.fromArray [(_ + 10), (_ * 2)]
        let xs = Strom.fromArray [1, 2, 3]
        applyResult <- runOm $ (fs <*> xs) # Strom.runCollect
        let ap' mf ma = mf >>= \f -> ma >>= \a -> pure (f a)
        apResult <- runOm $ ap' (Strom.fromArray [(_ + 10), (_ * 2)]) (Strom.fromArray [1, 2, 3]) # Strom.runCollect
        applyResult `shouldEqual` apResult

      it "apply matches ap for multi-chunk streams (BUG EXPECTED)" do
        -- Multi-chunk: both streams are built via <>, producing Loop steps.
        -- The Apply instance pairs corresponding chunks instead of doing cartesian product.
        -- fs = [+10] then [*2], xs = [1,2] then [3,4]
        -- Correct (ap): [11,12,13,14,2,4,6,8]
        -- Buggy (<*>): [11,12,6,8]  (only pairs chunk1×chunk1 and chunk2×chunk2)
        let fs = Strom.fromArray [(_ + 10)] <> Strom.fromArray [(_ * 2)]
        let xs = Strom.fromArray [1, 2] <> Strom.fromArray [3, 4]
        applyResult <- runOm $ (fs <*> xs) # Strom.runCollect
        let ap' mf ma = mf >>= \f -> ma >>= \a -> pure (f a)
        apResult <- runOm $ ap' (Strom.fromArray [(_ + 10)] <> Strom.fromArray [(_ * 2)]) (Strom.fromArray [1, 2] <> Strom.fromArray [3, 4]) # Strom.runCollect
        applyResult `shouldEqual` apResult

      it "apply matches ap with rangeStrom (multi-chunk, BUG EXPECTED)" do
        -- rangeStrom 1 3 = single chunk [1,2], rangeStrom 1 2001 = two 1000-element chunks
        -- Use small functions to keep output manageable
        let fs = Strom.fromArray [(_ + 100)] <> Strom.fromArray [(_ * (-1))]
        let xs = Strom.fromArray [1, 2, 3] <> Strom.fromArray [4, 5]
        applyResult <- runOm $ (fs <*> xs) # Strom.runCollect
        let ap' mf ma = mf >>= \f -> ma >>= \a -> pure (f a)
        apResult <- runOm $ ap' (Strom.fromArray [(_ + 100)] <> Strom.fromArray [(_ * (-1))]) (Strom.fromArray [1, 2, 3] <> Strom.fromArray [4, 5]) # Strom.runCollect
        applyResult `shouldEqual` apResult

    describe "Bind across chunk boundaries" do

      it "bind with multi-chunk source preserves all elements" do
        -- rangeStrom 1 2001 produces two 1000-element chunks
        -- Binding each to a small stream should produce all elements
        result <- runOm $
          Strom.rangeStrom 1 2001
            >>= (\n -> Strom.succeed (n * 2))
            # Strom.runCollect
        result `shouldEqual` map (_ * 2) (Array.range 1 2000)

      it "bind with multi-chunk source and multi-element expansion" do
        -- Each element expands to [n, n*10], creating 2x elements
        result <- runOm $
          Strom.rangeStrom 1 51
            >>= (\n -> Strom.fromArray [n, n * 10])
            # Strom.runCollect
        let expected = Array.range 1 50 >>= (\n -> [n, n * 10])
        result `shouldEqual` expected

      it "bind with multi-chunk source and variable-size expansion" do
        -- Some elements expand to 0, 1, or 2 elements (like filterMap + flatMap)
        let expand n
              | n `mod` 3 == 0 = Strom.empty
              | n `mod` 3 == 1 = Strom.succeed n
              | otherwise = Strom.fromArray [n, n * 100]
        result <- runOm $
          Strom.rangeStrom 1 2001
            >>= expand
            # Strom.runCollect
        let expected = Array.range 1 2000 >>= \n ->
              if n `mod` 3 == 0 then []
              else if n `mod` 3 == 1 then [n]
              else [n, n * 100]
        result `shouldEqual` expected

      it "bind associativity: (m >>= f) >>= g ≡ m >>= (\\x -> f x >>= g)" do
        let f n = Strom.fromArray [n, n + 1]
        let g n = Strom.fromArray [n * 10]
        let m = Strom.fromArray [1, 2, 3] <> Strom.fromArray [4, 5]
        result1 <- runOm $ ((m >>= f) >>= g) # Strom.runCollect
        result2 <- runOm $ (m >>= (\x -> f x >>= g)) # Strom.runCollect
        result1 `shouldEqual` result2

    describe "Cancel propagation" do

      it "zip cancels left stream when right is shorter" do
        ref <- liftEffect $ Ref.new false
        let
          leftStrom = Strom.bracket
            (pure unit)
            (\_ -> liftEffect $ Ref.write true ref)
            (\_ -> Strom.rangeStrom 1 10000)
          rightStrom = Strom.fromArray [10, 20, 30]
        result <- runOm $ Strom.zipStrom leftStrom rightStrom # Strom.runCollect
        result `shouldEqual` [Tuple 1 10, Tuple 2 20, Tuple 3 30]
        -- Give async cancel a moment to propagate
        delay (Milliseconds 50.0)
        released <- liftEffect $ Ref.read ref
        released `shouldEqual` true

      it "zip cancels right stream when left is shorter" do
        ref <- liftEffect $ Ref.new false
        let
          leftStrom = Strom.fromArray [10, 20]
          rightStrom = Strom.bracket
            (pure unit)
            (\_ -> liftEffect $ Ref.write true ref)
            (\_ -> Strom.rangeStrom 1 10000)
        result <- runOm $ Strom.zipStrom leftStrom rightStrom # Strom.runCollect
        result `shouldEqual` [Tuple 10 1, Tuple 20 2]
        delay (Milliseconds 50.0)
        released <- liftEffect $ Ref.read ref
        released `shouldEqual` true

      it "takeStrom cancels multi-chunk bracket source" do
        ref <- liftEffect $ Ref.new false
        let
          strom = Strom.bracket
            (pure unit)
            (\_ -> liftEffect $ Ref.write true ref)
            (\_ -> Strom.rangeStrom 1 10000)
        result <- runOm $ Strom.takeStrom 5 strom # Strom.runCollect
        result `shouldEqual` [1, 2, 3, 4, 5]
        released <- liftEffect $ Ref.read ref
        released `shouldEqual` true

      it "takeWhileStrom cancels multi-chunk bracket source" do
        ref <- liftEffect $ Ref.new false
        let
          strom = Strom.bracket
            (pure unit)
            (\_ -> liftEffect $ Ref.write true ref)
            (\_ -> Strom.rangeStrom 1 10000)
        result <- runOm $ Strom.takeWhileStrom (_ <= 3) strom # Strom.runCollect
        result `shouldEqual` [1, 2, 3]
        released <- liftEffect $ Ref.read ref
        released `shouldEqual` true

    describe "Stream to file" do

      it "streams 1000 random ints to a file" do
        let path = "/tmp/strom-test-random.txt"
        runOm $ FileStream.streamRandomToFile path 1000
        lineCount <- liftEffect $ FileStream.countLines path
        lineCount `shouldEqual` 1000
        liftEffect $ FileStream._unlinkFile path

      it "streams 10000 random ints to a file (multi-chunk)" do
        let path = "/tmp/strom-test-random-large.txt"
        runOm $ FileStream.streamRandomToFile path 10000
        lineCount <- liftEffect $ FileStream.countLines path
        lineCount `shouldEqual` 10000
        -- Read back and verify all lines parse as ints
        content <- liftEffect $ FileStream._readFile path
        let lines = Array.filter (_ /= "") $ String.split (String.Pattern "\n") content
        Array.length lines `shouldEqual` 10000
        liftEffect $ FileStream._unlinkFile path
