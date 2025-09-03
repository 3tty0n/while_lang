const fs = require("fs");

const { argv } = require('node:process');

(async () => {
  const bytes = fs.readFileSync(argv[2]);
  const imports = {
    env: {
      print: (x) => console.log(x >>> 0), // i32 を表示
    },
  };
  const { instance } = await WebAssembly.instantiate(bytes, imports);
  instance.exports.main(); // => 3
})();

