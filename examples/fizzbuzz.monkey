for (let i=0; i <= 100; i=i+1) {
    let div3 = i % 3 == 0;
    let div5 = i % 5 == 0;
    
    if (div3 && div5) {
        puts("FizzBuzz");
    } else if (div3) {
        puts("Fizz");
    } else if (div5) {
        puts("Buzz");
    } else {
        puts(i);
    }
}