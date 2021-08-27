const generator = require('./src/generator');

const args = process.argv.slice(2);

let spec;
let outdir = `output`;
let considerNulls = true;

for (let i = 0; i < args.length; i++) {
  switch (args[i]) {
    case '--help':
    case '-h':
      console.log(``);
      console.log(`RPGLE generator for OpenAPI 3.0 - WOB`);
      console.log(``);
      console.log(`\tGenerates a service program and headers to interact with APIs.`);
      console.log(`\tUses the Db2 for i HTTP functions and noxdb APIs for JSON parsing.`);
      console.log(``);
      console.log(`\t-n --nonull\t\tIgnore nullable keyword`);
      console.log(`\t-h --help\t\tThis help screen`);
      console.log(`\t-s --spec <spec.json>\tInput spec`);
      console.log(`\t-o --out\t\tOutput directory for RPGLE. Must exist`);
      console.log(``);
      break;

    case `-n`:
    case `-nonull`:
      considerNulls = false;
      break;

    case '--spec':
    case `-s`:
      spec = require(args[i + 1]);
      i++;
      break;
    
    case '--out':
    case `-o`:
      outdir = args[i + 1];
      i++;
      break;
  }
}

if (spec) {
  const gen = new generator(spec, {
    considerNulls,
  });
  gen.generate();
  gen.write(outdir);
}