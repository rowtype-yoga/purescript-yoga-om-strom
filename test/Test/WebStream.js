// Test FFI helpers for WebStream tests

// Collect all values from a ReadableStream into an array — returns Aff
export const _collectReadableStream = (stream) => (onError, onSuccess) => {
  const reader = stream.getReader();
  const values = [];
  function pump() {
    reader.read().then(
      ({ done, value }) => {
        if (done) {
          onSuccess(values);
        } else {
          values.push(value);
          pump();
        }
      },
      (err) => onError(err)
    );
  }
  pump();
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    reader.releaseLock();
    onCancelerSuccess();
  };
};

// Create a WritableStream that collects values into an array
export const _createWritableStreamCollector = () => {
  const values = [];
  const stream = new WritableStream({
    write(chunk) {
      values.push(chunk);
    }
  });
  return {
    stream: stream,
    getValues: () => values.slice()
  };
};

// Create a ReadableStream that produces incrementing integers.
// Each read() produces one value after a 1ms delay.
// Returns { stream, getProducedCount } — count grows per read,
// so after pump stops, the count stabilises.
export const _createCountingStream = () => {
  let count = 0;
  let cancelled = false;

  const stream = new ReadableStream({
    pull(controller) {
      if (cancelled) return;
      return new Promise(resolve => {
        setTimeout(() => {
          if (!cancelled) {
            count++;
            controller.enqueue(count);
          }
          resolve();
        }, 1);
      });
    },
    cancel() {
      cancelled = true;
    }
  });

  return {
    stream,
    getProducedCount: () => count
  };
};

// Check if a ReadableStream is locked (has an active reader)
export const _isStreamLocked = (stream) => () => {
  return stream.locked;
};

// Read N elements from a ReadableStream then cancel — returns EffectFnAff
export const _readNThenCancel = (stream) => (n) => (onError, onSuccess) => {
  const reader = stream.getReader();
  const values = [];
  function pump() {
    if (values.length >= n) {
      reader.cancel("read enough").then(
        () => { reader.releaseLock(); onSuccess(values); },
        (err) => onError(err)
      );
      return;
    }
    reader.read().then(
      ({ done, value }) => {
        if (done) {
          onSuccess(values);
        } else {
          values.push(value);
          pump();
        }
      },
      (err) => onError(err)
    );
  }
  pump();
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};
