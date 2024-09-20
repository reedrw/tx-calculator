# tx-calculator
A simple program to translate numbers into their Tic-Xenotation (TX) representation.

## What is Tic-Xenotation?
TX is a modulus-free numbering system that uses the following rules:
  - `:` for 2
  - `(n)` the nth prime number
  - Adjacent symbols are multiplied
  - Examples:
    * 9 = 3 * 3 = (2)(2) = (:)(:)
    * 55 = 5 * 11 = (3)(5) = ((2))((3)) = ((:))(((:)))

In this fashion, it is possible to represent every positive integer larger than 1 in TX. 0 and 1 are special cases represented by `((-P)):` and `(-P):` respectively.

## Usage
To TX:
```bash
$ tx 4872
:::(:)(::)(:((:)))
```

From TX:
```bash
$ tx ((:)(:))(((::)))
1357
```

## Try it out!
```bash
$ nix run github:reedrw/tx-calculator -- --help
```
