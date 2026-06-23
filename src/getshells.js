#!/usr/bin/env node

const events = require('events');
const fs = require('fs');
const readline = require('readline');

function increment(val) {
  if (val)
    return val + 1;
  else
    return 1;
}

(async function parsePasswd() {
  try {
    let shells = {};
    const rl = readline.createInterface({
      input: fs.createReadStream('passwd'),
      crlfDelay: Infinity
    });
    rl.on('line', (line) => {
      let shell = line.substring(line.lastIndexOf(":") + 1);
      shells[shell] = increment(shells[shell]);
    });
    await events.once(rl, 'close');

    for (const shell in shells) {
      console.log(`${shell}\t:\t${shells[shell]}`);
    }
  } catch (err) {
    console.log(err);
  }
})();
