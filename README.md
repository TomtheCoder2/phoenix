# Phoenix
## Programming Language
Based on the book [Crafting Interpreters](https://craftinginterpreters.com/) and [this repo](https://github.com/rctcwyvrn/rlox)

## Features
* Dynamic typing
* Automatic memory management
* Garbage collection
* First-class functions
* Closures
* Lexical scoping
* Tail recursion
* Pattern matching
* Algebraic data types
* Inheritance
* API for native functions written in Rust

## Examples
### Binary Search
```
fun binary_search(list, item) {
  var low = 0;
  var high = list.len() - 1;

  while low <= high {
    var mid = (low + high) / 2;
    var guess = list[mid];
    printf("low: {}, high: {}, mid: {}, guess: {}", low, high, mid, guess);

    if guess == item {
      return mid;
    } else if guess > item {
      high = mid - 1;
    } else {
      low = mid + 1;
    }
  }
  return nil;
}

var elements = [1, 3, 5, 7, 9];
var search = 7;
printf("element {} is at index: {}", search, binary_search([1, 3, 5, 7, 9], 7));
```

## TODO
* [ ] Add more examples
* [ ] Add more tests
* [ ] Add more documentation
* [ ] Add more error handling
* [ ] Fix lists (not working: access of lists in classes eg: `c.l[0] = 1;`)
* [ ] Fix REPL
* [ ] Fix modules (not working: classes)
* [x] Implement `[+-*/]=`, `i++` and `i--`
* [ ] Better "api" for native functions