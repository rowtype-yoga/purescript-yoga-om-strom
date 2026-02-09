import * as fs from "node:fs";

export const _writeFile = (path) => (content) => () => {
  fs.writeFileSync(path, content);
};

export const _appendFile = (path) => (content) => () => {
  fs.appendFileSync(path, content);
};

export const _readFile = (path) => () => {
  return fs.readFileSync(path, "utf-8");
};

export const _unlinkFile = (path) => () => {
  try { fs.unlinkSync(path); } catch (_) {}
};
