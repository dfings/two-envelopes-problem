#!/usr/bin/env deno run

// $ brew install typescript
// $ echo '{"compilerOptions": {"noImplicitAny": true}}' > tsconfig.json
// Deno:
// $ brew install deno
// Node:
// $ npm install -g ts-node
// $ npm install -g typescript

// $ ./envelopes.ts
// or
// $ tsc envelopes.ts --outFile envelopes.ts.js
// $ deno run ./envelopes.ts.js
// $ node ./envelopes.ts.js

const NUM_TRIALS = 10000;
const PRIOR_LOWER_MAX = 100;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately
// selects.
function singleTrial(pick: (value: number, other: number) => number): number {
  let lower_value = Math.random() * PRIOR_LOWER_MAX;
  let higher_value = 2 * lower_value;
  return Math.random() < 0.5 ? pick(lower_value, higher_value) : pick(higher_value, lower_value)
}

// Runs many trials at a given cutoff to approximate the expected value.
function multiTrial(cutoff: number): number {
  function pick(value: number, other: number) {
    return value >= cutoff ? value : other;
  }
  let total = 0;
  for (let i = 0; i < NUM_TRIALS; i++) {
    total += singleTrial(pick);
  }
  return total / NUM_TRIALS;
}

for (let cutoff = 0; cutoff <= 2 * PRIOR_LOWER_MAX; cutoff++) {
  console.log('cutoff=' + cutoff + ', expected_value=' + multiTrial(cutoff));
}
