const path = require('path');
const fs = require('fs/promises');


(async () => {
  const puzzlePath = process.argv[2];
  const puzzlePart = process.argv[3] ?? "1";
  const puzzleName = puzzlePath.split('/').at(-1);

  const puzzle = require(`./lib/js/src/${puzzlePath}.bs`);

  const inputPath = path.join(__dirname, 'inputs', `${puzzleName.toLowerCase()}.txt`);

  console.log('Solving puzzle: ', puzzleName);
  console.log('Part: ', puzzlePart);
  console.log('Reading input from: ', inputPath);
  const input = (await fs.readFile(inputPath)).toString();
  try {
    const output = await puzzle.solve(input, puzzlePart);
    console.log('\nGot output:\n');
    console.log(output);
  } catch (err) {
    console.error(err)
    process.exit(1)
  }
})();
