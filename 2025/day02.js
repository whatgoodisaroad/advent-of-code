const fs = require('fs');
const sample = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

function parse(i) {
  return i.split(',').map((r) => r.split('-').map((s) => parseInt(s, 10)));
}

const invalid2 = /^(\d+)\1$/;
const invalidN = /^(\d+)\1+$/;

function rangeMatches(ranges, pattern) {
  let s = 0;
  for (const [l, h] of ranges) {
    for (let i = l; i <= h; i++) {
      if (pattern.test(`${i}`)) {
        s += i
      }
    }
  }
  return s;
}

const file = fs.readFileSync('day02.input.txt', 'utf8');
console.log(`Part 1: ${rangeMatches(parse(file), invalid2)}`);
console.log(`Part 2: ${rangeMatches(parse(file), invalidN)}`);
