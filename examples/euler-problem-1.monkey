// Output: 233168
// https://projecteuler.net/problem=1

let sum = 0;
for (let i=1; i < 1000; i=i+1) {
    if (i % 3 == 0 || i % 5 == 0) {
        sum = sum + i;
    }
}
puts(sum);