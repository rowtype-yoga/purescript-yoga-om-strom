module Test.WebStream where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, killFiber)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Yoga.Om (Om)
import Yoga.Om as Om
import Yoga.Om.Strom as Strom
import Yoga.Om.Strom.WebStream as WS
import Data.Time.Duration (Milliseconds(..))

-- Helper to run Om in tests (same as in Strom tests)
runOm :: forall a. Om {} () a -> Aff a
runOm om = Om.runOm {} { exception: \err -> liftEffect (Exception.throwException err) } om

-- FFI helpers for testing
foreign import _collectReadableStream :: forall a. WS.ReadableStream a -> EffectFnAff (Array a)
foreign import _createWritableStreamCollector :: forall a. Effect { stream :: WS.WritableStream a, getValues :: Effect (Array a) }
foreign import _readNThenCancel :: forall a. WS.ReadableStream a -> Int -> EffectFnAff (Array a)
foreign import _createCountingStream :: Effect { stream :: WS.ReadableStream Int, getProducedCount :: Effect Int }
foreign import _isStreamLocked :: forall a. WS.ReadableStream a -> Effect Boolean

collectReadableStream :: forall a. WS.ReadableStream a -> Aff (Array a)
collectReadableStream = fromEffectFnAff <<< _collectReadableStream

readNThenCancel :: forall a. WS.ReadableStream a -> Int -> Aff (Array a)
readNThenCancel stream n = fromEffectFnAff (_readNThenCancel stream n)

spec :: Spec Unit
spec = do
  describe "WebStream" do

    describe "toReadableStream" do

      it "converts empty Strom to empty ReadableStream" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.empty :: Strom.Strom {} () Int)
        result <- collectReadableStream rs
        result `shouldEqual` ([] :: Array Int)

      it "converts single-element Strom to ReadableStream" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.succeed 42)
        result <- collectReadableStream rs
        result `shouldEqual` [ 42 ]

      it "converts multi-element Strom to ReadableStream" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.fromArray [ 1, 2, 3, 4, 5 ])
        result <- collectReadableStream rs
        result `shouldEqual` [ 1, 2, 3, 4, 5 ]

      it "converts range Strom to ReadableStream" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.rangeStrom 1 6)
        result <- collectReadableStream rs
        result `shouldEqual` [ 1, 2, 3, 4, 5 ]

      it "converts transformed Strom to ReadableStream" do
        let strom = Strom.fromArray [ 1, 2, 3 ] # Strom.mapStrom (_ * 10)
        rs <- liftEffect $ WS.toReadableStream {} strom
        result <- collectReadableStream rs
        result `shouldEqual` [ 10, 20, 30 ]

    describe "fromReadableStream" do

      it "converts ReadableStream back to Strom" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.fromArray [ 10, 20, 30 ])
        let strom = WS.fromReadableStream rs :: Strom.Strom {} () Int
        result <- runOm $ Strom.runCollect strom
        result `shouldEqual` [ 10, 20, 30 ]

      it "converts empty ReadableStream to empty Strom" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.empty :: Strom.Strom {} () Int)
        let strom = WS.fromReadableStream rs :: Strom.Strom {} () Int
        result <- runOm $ Strom.runCollect strom
        result `shouldEqual` ([] :: Array Int)

    describe "fromReadableStreamWith" do

      it "allows custom chunk size" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.fromArray [ 1, 2, 3, 4, 5 ])
        let strom = WS.fromReadableStreamWith { chunkSize: 2 } rs :: Strom.Strom {} () Int
        result <- runOm $ Strom.runCollect strom
        result `shouldEqual` [ 1, 2, 3, 4, 5 ]

    describe "roundtrip" do

      it "Strom → ReadableStream → Strom preserves elements" do
        let original = [ 1, 2, 3, 4, 5 ]
        rs <- liftEffect $ WS.toReadableStream {} (Strom.fromArray original)
        let strom = WS.fromReadableStream rs :: Strom.Strom {} () Int
        result <- runOm $ Strom.runCollect strom
        result `shouldEqual` original

      it "roundtrip with transformations" do
        let original = Strom.fromArray [ 1, 2, 3 ] # Strom.filterStrom (_ > 1) # Strom.mapStrom (_ * 2)
        rs <- liftEffect $ WS.toReadableStream {} original
        let strom = WS.fromReadableStream rs :: Strom.Strom {} () Int
        result <- runOm $ Strom.runCollect strom
        result `shouldEqual` [ 4, 6 ]

      it "large roundtrip preserves all 10000 elements" do
        let original = Array.range 1 10000
        rs <- liftEffect $ WS.toReadableStream {} (Strom.rangeStrom 1 10001)
        let strom = WS.fromReadableStream rs :: Strom.Strom {} () Int
        result <- runOm $ Strom.runCollect strom
        result `shouldEqual` original

    describe "cancellation" do

      it "cancelling ReadableStream does not hang" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.rangeStrom 1 1000000)
        -- Read 5 elements then cancel
        result <- readNThenCancel rs 5
        Array.length result `shouldEqual` 5

    describe "runWritable" do

      it "writes Strom elements to WritableStream" do
        { stream: ws, getValues } <- liftEffect _createWritableStreamCollector
        runOm $ WS.runWritable ws (Strom.fromArray [ 1, 2, 3 ])
        result <- liftEffect getValues
        result `shouldEqual` [ 1, 2, 3 ]

      it "handles empty Strom to WritableStream" do
        { stream: ws, getValues } <- liftEffect (_createWritableStreamCollector :: Effect { stream :: WS.WritableStream Int, getValues :: Effect (Array Int) })
        runOm $ WS.runWritable ws (Strom.empty :: Strom.Strom {} () Int)
        result <- liftEffect getValues
        result `shouldEqual` ([] :: Array Int)

    describe "pipeThrough" do

      it "transforms ReadableStream through Strom pipeline" do
        -- Create a source ReadableStream
        source <- liftEffect $ WS.toReadableStream {} (Strom.fromArray [ 1, 2, 3, 4, 5 ])
        -- Pipe through a Strom transformation
        transformed <- liftEffect $ WS.pipeThrough {} (Strom.filterStrom (_ > 2) >>> Strom.mapStrom (_ * 10)) source
        -- Collect the result
        result <- collectReadableStream transformed
        result `shouldEqual` [ 30, 40, 50 ]

      it "pipes through with take" do
        source <- liftEffect $ WS.toReadableStream {} (Strom.rangeStrom 1 100)
        transformed <- liftEffect $ WS.pipeThrough {} (Strom.takeStrom 3) source
        result <- collectReadableStream transformed
        result `shouldEqual` [ 1, 2, 3 ]

      it "cancelling piped stream releases reader" do
        source <- liftEffect $ WS.toReadableStream {} (Strom.rangeStrom 1 1000000)
        transformed <- liftEffect $ WS.pipeThrough {} (Strom.mapStrom (_ * 2)) source
        -- Read 5 elements then cancel
        result <- readNThenCancel transformed 5
        Array.length result `shouldEqual` 5

    describe "cancellation (batch read)" do

      it "killing fiber during batch read stops the pump loop" do
        { stream, getProducedCount } <- liftEffect _createCountingStream
        -- Start reading in a fiber — chunkSize 10000 means pump loops many times
        let strom = WS.fromReadableStreamWith { chunkSize: 10000 } stream :: Strom.Strom {} () Int
        fiber <- forkAff $ runOm $ Strom.runCollect strom
        -- Let it produce some values (~50 reads at 1ms each)
        delay (Milliseconds 50.0)
        -- Kill the fiber — the cancelled flag stops the pump from further reads
        killFiber (Exception.error "test cancel") fiber
        -- Record count shortly after kill (allow one in-flight read to complete)
        delay (Milliseconds 10.0)
        countAtKill <- liftEffect getProducedCount
        -- Wait and check that the producer has stopped (count stabilised)
        delay (Milliseconds 100.0)
        countLater <- liftEffect getProducedCount
        -- With cancellation fix, the pump stops. At most 1 in-flight read
        -- may complete after the flag is set.
        (countLater - countAtKill) `shouldSatisfy` (_ < 5)

    describe "useReadableStream" do

      it "releases reader after full consumption" do
        rs <- liftEffect $ WS.toReadableStream {} (Strom.fromArray [ 1, 2, 3 ])
        result <- runOm $ WS.useReadableStream rs \strom ->
          Strom.runCollect strom
        result `shouldEqual` [ 1, 2, 3 ]

      it "releases reader after partial consumption (takeStrom)" do
        { stream, getProducedCount: _ } <- liftEffect _createCountingStream
        result <- runOm $ WS.useReadableStreamWith { chunkSize: 10 } stream \strom ->
          Strom.runCollect (Strom.takeStrom 5 strom)
        Array.length result `shouldEqual` 5
        -- Wait for async reader.cancel() to complete
        delay (Milliseconds 50.0)
        -- Stream should be unlocked after useReadableStream returns
        locked <- liftEffect $ _isStreamLocked stream
        locked `shouldEqual` false

      it "releases reader on fiber cancellation" do
        { stream, getProducedCount: _ } <- liftEffect _createCountingStream
        -- Fork useReadableStream that reads forever
        fiber <- forkAff $ runOm $ WS.useReadableStreamWith { chunkSize: 10 } stream \strom ->
          Strom.runCollect strom
        -- Let it start reading
        delay (Milliseconds 30.0)
        -- Kill the fiber — bracket cleanup should cancel the reader
        killFiber (Exception.error "test cancel") fiber
        -- Wait for async reader.cancel() to complete
        delay (Milliseconds 50.0)
        locked <- liftEffect $ _isStreamLocked stream
        locked `shouldEqual` false

    describe "cancelStrom integration" do

      it "cancelling ReadableStream releases bracket resources" do
        ref <- liftEffect $ Ref.new false
        let strom = Strom.bracket (pure unit) (\_ -> liftEffect $ Ref.write true ref) (\_ -> Strom.rangeStrom 1 1000000)
        rs <- liftEffect $ WS.toReadableStream {} strom
        _ <- readNThenCancel rs 5
        delay (Milliseconds 50.0)
        released <- liftEffect $ Ref.read ref
        released `shouldEqual` true
