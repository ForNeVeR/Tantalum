Tantalum
========

About
-----
Tantalum is a simple algebraic computation system written in F#.

Structure overview
------------------
Tantalum project consists of two assemblies:

### Tantalum
This is a library for possible use by external code. It contains main Tantalum
types.

### Voice
Contains the input module for string interpreting.

Usage
-----
Try to compile Tantalum application and run its executable. It'll welcome you
with simple `> ` prompt. Feel free to enter simple algebraic expressions there
and you'll get the answers.

For example, consider such simple session:

    > 2+2*0
    (2 + (2 * 0)) = (2 + 0) = 2b

Here you can see the original form of expression `(2 + (2 * 0))`, its
simplified form `(2 + 0)` and the result `2b`.

Note that `b` letter stands for so called "binary" representation of
calculation result, as opposed to its symbolic representation.

3rd-party libraries
-------------------
Tantalum uses following 3-rd party libraries:

* FParsec by Stephan Tolksdorf [http://www.quanttec.com/fparsec/]
