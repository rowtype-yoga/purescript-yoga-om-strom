module Yoga.Om.Strom.WebStream
  ( ReadableStream
  , WritableStream
  , TransformStream
  , toReadableStream
  , fromReadableStream
  , fromReadableStreamWith
  , useReadableStream
  , useReadableStreamWith
  , runWritable
  , pipeThrough
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, bracket, killFiber, launchAff, try)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Yoga.Om (Om)
import Yoga.Om as Om
import Yoga.Om.Strom (Strom, mkStrom, runStrom)

--------------------------------------------------------------------------------
-- Foreign Types
--------------------------------------------------------------------------------

foreign import data ReadableStream :: Type -> Type
foreign import data WritableStream :: Type -> Type
foreign import data TransformStream :: Type -> Type -> Type

-- Internal opaque types for FFI
foreign import data Controller :: Type -> Type
foreign import data Reader :: Type -> Type
foreign import data Writer :: Type -> Type
foreign import data Promise :: Type -> Type
foreign import data BatchResult :: Type -> Type

--------------------------------------------------------------------------------
-- Foreign Imports
--------------------------------------------------------------------------------

-- ReadableStream construction
foreign import _newReadableStream
  :: forall a
   . (Controller a -> Effect (Promise Unit))
  -> Effect Unit
  -> Effect (ReadableStream a)

foreign import _enqueue :: forall a. Controller a -> a -> Effect Unit
foreign import _closeController :: forall a. Controller a -> Effect Unit
foreign import _errorController :: forall a. Controller a -> Exception.Error -> Effect Unit

-- Promise construction from callbacks
foreign import _makePromise
  :: forall a
   . ((a -> Effect Unit) -> (Exception.Error -> Effect Unit) -> Effect Unit)
  -> Effect (Promise a)

-- Reader operations
foreign import _getReader :: forall a. ReadableStream a -> Effect (Reader a)
foreign import _releaseReader :: forall a. Reader a -> Effect Unit
foreign import _cancelReaderImpl :: forall a. Reader a -> EffectFnAff Unit

-- Batch reader operations
foreign import _readBatchImpl :: forall a. Reader a -> Int -> EffectFnAff (BatchResult a)
foreign import _batchResultValues :: forall a. BatchResult a -> Array a
foreign import _batchResultDone :: forall a. BatchResult a -> Boolean

-- Error conversion
foreign import _variantToError :: forall err. Variant err -> Exception.Error

-- Writer operations
foreign import _getWriter :: forall a. WritableStream a -> Effect (Writer a)
foreign import _writeWriterImpl :: forall a. Writer a -> a -> EffectFnAff Unit
foreign import _closeWriterImpl :: forall a. Writer a -> EffectFnAff Unit
foreign import _writerReadyImpl :: forall a. Writer a -> EffectFnAff Unit

-- TransformStream
foreign import _newTransformStream :: forall a b. Effect (TransformStream a b)
foreign import _transformReadable :: forall a b. TransformStream a b -> ReadableStream b
foreign import _transformWritable :: forall a b. TransformStream a b -> WritableStream a

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

readBatch :: forall a. Reader a -> Int -> Aff (BatchResult a)
readBatch reader batchSize = fromEffectFnAff (_readBatchImpl reader batchSize)

writeWriter :: forall a. Writer a -> a -> Aff Unit
writeWriter writer value = fromEffectFnAff (_writeWriterImpl writer value)

closeWriter :: forall a. Writer a -> Aff Unit
closeWriter writer = fromEffectFnAff (_closeWriterImpl writer)

writerReady :: forall a. Writer a -> Aff Unit
writerReady writer = fromEffectFnAff (_writerReadyImpl writer)

--------------------------------------------------------------------------------
-- Strom → ReadableStream (shared core)
--------------------------------------------------------------------------------

-- | Internal: builds a ReadableStream from a Strom with a cleanup action
-- | that runs on close, error, or cancel. The cleanup action must be
-- | idempotent (it may be called more than once).
stromToReadableStream :: forall a ctx err. Effect Unit -> ctx -> Strom ctx err a -> Effect (ReadableStream a)
stromToReadableStream cleanup ctx strom = do
  stromRef <- Ref.new strom
  fiberRef <- Ref.new (Nothing :: Maybe (Fiber Unit))
  cancelledRef <- Ref.new false
  let
    -- Keep pulling until we enqueue data or the stream ends.
    -- ReadableStream does NOT re-call pull when nothing is enqueued,
    -- so we must loop internally for empty chunks (e.g. from filterStrom).
    pullLoop :: Controller a -> Aff Unit
    pullLoop controller = do
      cancelled <- liftEffect $ Ref.read cancelledRef
      when (not cancelled) do
        currentStrom <- liftEffect $ Ref.read stromRef
        result <- Om.runReader ctx (runStrom currentStrom)
        case result of
          Left omErr -> liftEffect do
            cleanup
            _errorController controller (_variantToError omErr)
          Right step -> case step of
            Done Nothing -> liftEffect do
              cleanup
              _closeController controller
            Done (Just chunk) -> liftEffect do
              traverse_ (_enqueue controller) chunk
              cleanup
              _closeController controller
            Loop (Tuple maybeChunk next) -> do
              liftEffect $ Ref.write next stromRef
              case maybeChunk of
                Nothing -> pullLoop controller
                Just chunk -> liftEffect $ traverse_ (_enqueue controller) chunk

    pull :: Controller a -> Effect (Promise Unit)
    pull controller =
      _makePromise \resolve _reject -> do
        fiber <- launchAff do
          result <- try (pullLoop controller)
          liftEffect do
            case result of
              -- pullLoop already signalled the controller (error/close) before
              -- any Aff-level throw, or the controller is already in a terminal
              -- state. Avoid calling _errorController again — it would throw on
              -- an already-errored/closed controller and the Promise would hang.
              Left _err -> cleanup
              Right _ -> pure unit
            resolve unit
        Ref.write (Just fiber) fiberRef

    cancel :: Effect Unit
    cancel = do
      Ref.write true cancelledRef
      -- Kill the fiber first so the JS cancelled flag in _readBatchImpl is set
      -- before cleanup calls releaseLock (which rejects pending reads).
      maybeFiber <- Ref.read fiberRef
      case maybeFiber of
        Nothing -> pure unit
        Just fiber ->
          void $ launchAff $ killFiber (Exception.error "cancelled") fiber
      currentStrom <- Ref.read stromRef
      pure unit -- TODO: cancelStrom currentStrom
      cleanup

  _newReadableStream pull cancel

--------------------------------------------------------------------------------
-- Strom → ReadableStream (public)
--------------------------------------------------------------------------------

-- | Convert a Strom to a ReadableStream. Elements are enqueued individually.
-- | Requires ctx to run the Om effects. Errors become stream errors with
-- | preserved error information. Supports cancellation — when the consumer
-- | cancels the ReadableStream, the internal fiber is killed.
toReadableStream :: forall a ctx err. ctx -> Strom ctx err a -> Effect (ReadableStream a)
toReadableStream = stromToReadableStream (pure unit)

--------------------------------------------------------------------------------
-- ReadableStream → Strom
--------------------------------------------------------------------------------

-- | Default batch size for `fromReadableStream` (matches rangeStrom's chunk size).
defaultChunkSize :: Int
defaultChunkSize = 1000

-- | Convert a ReadableStream to a Strom using batched reads.
-- | Each pull reads up to 1000 elements from the underlying reader in a tight
-- | JS loop, producing properly-sized chunks that match Strom's batch model.
-- | The reader is released when the stream ends.
-- |
-- | Note: if the consumer stops pulling early (e.g. via `takeStrom`), the
-- | reader lock is not released until GC. Use `useReadableStream` for
-- | guaranteed cleanup on abandonment.
fromReadableStream :: forall a ctx err. ReadableStream a -> Strom ctx err a
fromReadableStream = fromReadableStreamWith { chunkSize: defaultChunkSize }

-- | Convert a ReadableStream to a Strom with a configurable chunk (batch) size.
-- | Larger values reduce per-chunk overhead; smaller values reduce latency.
fromReadableStreamWith :: forall a ctx err. { chunkSize :: Int } -> ReadableStream a -> Strom ctx err a
fromReadableStreamWith { chunkSize } stream = mkStrom do
  reader <- liftEffect $ _getReader stream
  runStrom (readerToStrom chunkSize reader)

-- | Internal: pull from a Reader in batches, releasing the reader on completion.
readerToStrom :: forall a ctx err. Int -> Reader a -> Strom ctx err a
readerToStrom batchSize reader =
  readerToStromWithCleanupAndSize batchSize (_releaseReader reader) reader

--------------------------------------------------------------------------------
-- Strom → WritableStream sink
--------------------------------------------------------------------------------

-- | Drain a Strom into a WritableStream. Resolves when the stream is fully written.
-- | Backpressure comes from the writer's ready promise.
runWritable :: forall a ctx err. WritableStream a -> Strom ctx err a -> Om ctx err Unit
runWritable writable strom = do
  writer <- liftEffect $ _getWriter writable
  Om.handleErrors'
    (\err -> do
      -- Try to close on error; swallow secondary failures
      _ <- liftAff $ try $ closeWriter writer
      throwError err
    )
    do
      go writer strom
      liftAff $ closeWriter writer
  where
  go :: Writer a -> Strom ctx err a -> Om ctx err Unit
  go writer s = do
    step <- runStrom s
    case step of
      Done Nothing -> pure unit
      Done (Just chunk) -> writeChunk writer chunk
      Loop (Tuple maybeChunk next) -> do
        maybe (pure unit) (writeChunk writer) maybeChunk
        go writer next

  writeChunk :: Writer a -> Array a -> Om ctx err Unit
  writeChunk writer chunk = traverse_ (writeElement writer) chunk

  writeElement :: Writer a -> a -> Om ctx err Unit
  writeElement writer value = liftAff do
    writerReady writer
    writeWriter writer value

--------------------------------------------------------------------------------
-- Piping utilities
--------------------------------------------------------------------------------

-- | Pipe a ReadableStream through a Strom transformation, producing a new
-- | ReadableStream. This is the primary integration point: take web streams,
-- | transform them with Strom combinators, and produce a web stream.
-- |
-- | Reader cleanup: the reader acquired from the source ReadableStream is
-- | released when the transformed stream ends naturally or when the output
-- | ReadableStream is cancelled by the consumer.
pipeThrough
  :: forall a b ctx err
   . ctx
  -> (Strom ctx err a -> Strom ctx err b)
  -> ReadableStream a
  -> Effect (ReadableStream b)
pipeThrough ctx transform readable = do
  reader <- _getReader readable
  -- Idempotent release — safe to call multiple times
  releasedRef <- Ref.new false
  let
    releaseOnce :: Effect Unit
    releaseOnce = do
      alreadyReleased <- Ref.read releasedRef
      when (not alreadyReleased) do
        Ref.write true releasedRef
        _releaseReader reader

    inputStrom = readerToStromWithCleanupAndSize defaultChunkSize releaseOnce reader

  stromToReadableStream releaseOnce ctx (transform inputStrom)

-- | Internal: pull from a Reader in batches with configurable chunk size,
-- | calling a cleanup action on stream completion.
readerToStromWithCleanupAndSize :: forall a ctx err. Int -> Effect Unit -> Reader a -> Strom ctx err a
readerToStromWithCleanupAndSize batchSize cleanup reader = go -- TODO: addCancel cleanup go
  where
  go :: Strom ctx err a
  go = mkStrom do
    result <- liftAff $ readBatch reader batchSize
    let values = _batchResultValues result
    let done = _batchResultDone result
    if done then
      if Array.null values then pure $ Done Nothing
      else pure $ Done $ Just values
    else
      pure $ Loop $ Tuple (Just values) go

--------------------------------------------------------------------------------
-- Bracketed ReadableStream consumption
--------------------------------------------------------------------------------

-- | Consume a ReadableStream with guaranteed reader cleanup via `Aff.bracket`.
-- | The reader is released when the callback completes, errors, or the fiber
-- | is cancelled — unlike `fromReadableStream` which leaks the reader on
-- | early abandonment.
-- |
-- | ```purescript
-- | useReadableStream stream \strom -> do
-- |   Strom.runCollect (Strom.takeStrom 5 strom)
-- | ```
useReadableStream
  :: forall a b ctx err
   . ReadableStream a
  -> (Strom ctx err a -> Om ctx err b)
  -> Om ctx err b
useReadableStream = useReadableStreamWith { chunkSize: defaultChunkSize }

-- | Like `useReadableStream` but with a configurable chunk (batch) size.
useReadableStreamWith
  :: forall a b ctx err
   . { chunkSize :: Int }
  -> ReadableStream a
  -> (Strom ctx err a -> Om ctx err b)
  -> Om ctx err b
useReadableStreamWith { chunkSize } stream callback = do
  ctx <- Om.ask
  result <- liftAff $ bracket
    (liftEffect do
      reader <- _getReader stream
      releasedRef <- Ref.new false
      pure { reader, releasedRef })
    (\{ reader, releasedRef } -> do
      alreadyReleased <- liftEffect $ Ref.read releasedRef
      when (not alreadyReleased) do
        liftEffect $ Ref.write true releasedRef
        fromEffectFnAff (_cancelReaderImpl reader))
    (\{ reader, releasedRef } -> do
      let releaseOnce = do
            alreadyReleased <- Ref.read releasedRef
            when (not alreadyReleased) do
              Ref.write true releasedRef
              _releaseReader reader
          inputStrom = readerToStromWithCleanupAndSize chunkSize releaseOnce reader
      Om.runReader ctx (callback inputStrom))
  case result of
    Left err -> throwError err
    Right value -> pure value
