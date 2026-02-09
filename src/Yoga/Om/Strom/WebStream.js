// FFI bindings for Web Streams API interop

// Create a ReadableStream with a pull-based source
// pullFn :: Controller -> Effect Promise
export const _newReadableStream = (pullFn) => (cancelFn) => () => {
  return new ReadableStream({
    pull(controller) {
      return pullFn(controller)();
    },
    cancel() {
      cancelFn();
    }
  });
};

// Enqueue a value into a ReadableStreamDefaultController
export const _enqueue = (controller) => (value) => () => {
  controller.enqueue(value);
};

// Close a ReadableStreamDefaultController
export const _closeController = (controller) => () => {
  controller.close();
};

// Signal an error on a ReadableStreamDefaultController
export const _errorController = (controller) => (err) => () => {
  controller.error(err);
};

// Create a Promise from resolve/reject callbacks.
// Takes an Effect-wrapped function that receives resolve and reject as Effect callbacks.
// makePromise :: ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Effect (Promise a)
export const _makePromise = (k) => () => {
  return new Promise((resolve, reject) => {
    k((a) => () => resolve(a))((e) => () => reject(e))();
  });
};

// Get a reader from a ReadableStream
export const _getReader = (stream) => () => {
  return stream.getReader();
};

// Release the reader lock
export const _releaseReader = (reader) => () => {
  reader.releaseLock();
};

// Get a writer from a WritableStream
export const _getWriter = (stream) => () => {
  return stream.getWriter();
};

// Write a value using a writer — returns EffectFnAff
export const _writeWriterImpl = (writer) => (value) => (onError, onSuccess) => {
  writer.write(value).then(
    () => onSuccess(),
    (err) => onError(err)
  );
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};

// Close a writer — returns EffectFnAff
export const _closeWriterImpl = (writer) => (onError, onSuccess) => {
  writer.close().then(
    () => onSuccess(),
    (err) => onError(err)
  );
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};

// Wait for writer.ready — returns EffectFnAff
export const _writerReadyImpl = (writer) => (onError, onSuccess) => {
  writer.ready.then(
    () => onSuccess(),
    (err) => onError(err)
  );
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};

// Create a new TransformStream
export const _newTransformStream = () => {
  return new TransformStream();
};

// Get the readable side of a TransformStream
export const _transformReadable = (ts) => {
  return ts.readable;
};

// Get the writable side of a TransformStream
export const _transformWritable = (ts) => {
  return ts.writable;
};

// Batch-read up to N elements from a Reader in one Aff call.
// Returns { values: Array, done: Boolean }
// Cancellation: sets a flag that stops the pump loop and suppresses callbacks.
export const _readBatchImpl = (reader) => (batchSize) => (onError, onSuccess) => {
  let cancelled = false;
  const values = [];
  function pump() {
    if (cancelled) return;
    if (values.length >= batchSize) {
      onSuccess({ values, done: false });
      return;
    }
    reader.read().then(
      ({ done, value }) => {
        if (cancelled) return;
        if (done) {
          onSuccess({ values, done: true });
        } else {
          values.push(value);
          pump();
        }
      },
      (err) => { if (!cancelled) onError(err); }
    );
  }
  pump();
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    cancelled = true;
    onCancelerSuccess();
  };
};

// Extract the values array from a batch result
export const _batchResultValues = (result) => {
  return result.values;
};

// Check if a batch result indicates stream end
export const _batchResultDone = (result) => {
  return result.done;
};

// Cancel a reader and release the lock (async, safe even with pending reads).
// reader.cancel() resolves pending reads, then releaseLock() drops the lock.
export const _cancelReaderImpl = (reader) => (onError, onSuccess) => {
  reader.cancel().then(
    () => { reader.releaseLock(); onSuccess(); },
    (err) => onError(err)
  );
  return (cancelError, onCancelerError, onCancelerSuccess) => { onCancelerSuccess(); };
};

// Convert an Om Variant error to a JS Error, preserving info.
// Variant internal shape: { type: string, value: ... }
// If value is already an Error (exception tag), pass it through.
// Otherwise create an Error with descriptive message and attach original variant.
export const _variantToError = (variant) => {
  if (variant && typeof variant.type === "string") {
    if (variant.type === "exception" && variant.value instanceof Error) {
      return variant.value;
    }
    const tag = variant.type;
    let msg;
    try {
      msg = "Stream error [" + tag + "]: " + JSON.stringify(variant.value);
    } catch (_e) {
      msg = "Stream error [" + tag + "]";
    }
    const err = new Error(msg);
    err.originalVariant = variant;
    return err;
  }
  return new Error("Stream error: " + String(variant));
};
