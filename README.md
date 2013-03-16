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
modules and types.

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

License
-------
Copyright (c) 2011-2013 Friedrich von Never

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

3rd-party libraries
-------------------
Tantalum uses following 3-rd party libraries:

* FParsec by Stephan Tolksdorf [http://www.quanttec.com/fparsec/]
