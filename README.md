# 🎄🎅 Advent of Code 2021

Written in ReScript, running on Node.js.

## Running

Each solution is a module that has a `solve` function of type `string => string | Promise<string>`.

To run a solution, first install the dependencies and build the code:

```
npm i
npm run build
```

Then pass it's path to `run.js` (without the .res file extension) eg.

```
node run.js example/Demo
```
