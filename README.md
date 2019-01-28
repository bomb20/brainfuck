# brainfuck

This is another implementation of the famous `brainfuck` programming-language. 
It reads brainfuck-sourcecode from a file and reads input from stdin.

This Interpreter was written in the process of a Haskell talk and is mainly
ment for educational purpose.

## Installation

Simply install via stack
```bash
git clone https://github.com/bomb20/brainfuck
cd brainfuck
stack install
```

## Usage

### Synopsis

```
brainfuck-exe <source-file> [debug]
```

### Debug mode

in debug mode ```brainfuck``` simply prints out a representtion of the tape and
the programm with every step of execution. Best is to srite this to file
directly.
