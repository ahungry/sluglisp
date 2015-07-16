mexpr
=====

[![Build Status](https://travis-ci.org/tmccombs/mexpr.svg)](https://travis-ci.org/tmccombs/mexpr)
[![Join the chat at https://gitter.im/tmccombs/mexpr](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/tmccombs/mexpr?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Macro for Common Lisp to allow infix syntax for mathematical expressions.

The mexpr package contains the `infix` macro which converts infix expressions into Lisp S-Expressions.

Installation
------------
The easiest way to install mexpr is with quicklisp.

```lisp
(ql:quickload :mexpr)
```

Examples
--------

The following are examples of how the `infix` macro can be used:

```lisp
CL-USER> (infix 3 + 4)
7
CL-USER> (infix [ 3 + 4 ] * 5)
35
CL-USER (infix 3 + 4 * 5)
23
CL-USER> (let ((x 0))
	   		   (infix [ 4 + (1+ x) ] / (sqrt 4)))
2.5
CL-USER> (infix 4 + 4 < 7 or 2 > 6 / 16)
T
CL-USER> (infix 5 = 6)
NIL
CL-USER> (infix 5 = 5)
T
CL-USER> (infix 2 expt 5)
32
```

You can use `defop` to add new operators:

```lisp
CL-USER> (defop union 10) ; use function name and precedence
CL-USER> (infix '(1 2) union '(2 3))
(2 1 3 4)
CL-USER> (defop ++ 100 append) ; use operator symbol, precedence, and defition symbol
CL-USER> (infix '(1 2) ++ '(3 4))
(1 2 3 4)
CL-USER> (defop p 110 (lambda (a b) (+ (* a a) (* b b)))) ; use lambda for definition
CL-USER> (infix 3 p 4)
25
```

You can use a reader macro to make it a little simpler:

```lisp
CL-USER> (enable-infix-syntax) ; equivalent to (cl-syntax:use-syntax :mexpr)
CL-USER> #n(3 + 4 ** 2)
19
```

### Notes:
1. There always needs to be whitespace between operands and operators.
2. Variables and other forms can be used as operands.
3. Operators have an associated precedence. Higher precedence operators are performed first.
4. Operators of equal precedence are evaluated left to right.
5. [ and ] are used for grouping expressions (overriding precedence).

Usage
-----

The `mexpr` (or more verbose `bytecurry.mexpr`) package contains two main macros.

The `infix` macro parses it's arguments as an infix expression and produces the corresponding s-expression. Each argument
is evaluated as one of the following forms:

  - *Grouping*: The special forms `[` and `]` are used for grouping expressions. (Parentheses were already taken.)
  - *Operator*: An operator is a symbol that has been registered using the `defop` macro. It represents a binary operation.
  - *Operand*: An operand is any form which is not an operator. This means that normal prefix forms can be embedded in the infix expression.

The `infix` macro can detect some syntax errors, in which case it will create a `syntax-error` condition. The type of the
syntax error can be obtained with `syntax-error-type`. Unfortunately, at the moment some invalid forms simply produce strange results, such as a transposition of a operator and operand.

---------------------------------------------------------------------------------

The `defop` macro can be used to define new operators. It takes two parameters, the first is the unquoted symbol of the
operator, the second is the desired precedence of the operator [(see below for precedence table)](#precedence). The symbol
should correspond to a function or macro which can accept exactly two arguments (although it may have more optional arguments).

---------------------------------------------------------------------------------

The function `infix-reader` is macro dispatch function which is available to the user to use in
any reader macro he/she desires. The package also registers the "#n" dispatch with cl-syntax, so
you can enable syntax of the form `#n(<expr>)` with `(cl-syntax:use-syntax :mexpr)`. Alternatively,
`enable-infix-syntax` is a wrapper around `cl-syntax:use-syntax`.

Precedence
----------

Unlike prefix and postfix notations, infix notation uses operator precedence to determine the order of evaluation.
`mexpr` uses a numeric precedence system, where the precedence of an operator is a positive integer. A higher number
corresponds to a higher precedence. The precedence of the default operators is given below:

| Operator   | Precedence | Translation of `a <op> b`
|:----------:|-----------:|:--------------------------
| **         | 110        | `(expt a b)`
| expt       | 110        | `(expt a b)`
| *          | 100        | `(* a b)`
| /          | 100        | `(/ a b)`
| %          | 100        | `(mod a b)`
| mod        | 100        | `(mod a b)`
| rem        | 100        | `(rem a b)`
| +          | 90         | `(+ a b)`
| -          | 90         | `(- a b)`
| ash        | 80         | `(ash a b)`
| <<         | 80         | `(ash a b)`
| >>         | 80         | `(ash a (- b))`
| <          | 70         | `(< a b)`
| >          | 70         | `(> a b)`
| <=         | 70         | `(<= a b)`
| >=         | 70         | `(>= a b)`
| =          | 60         | `(= a b)`
| /=         | 60         | `(/= a b)`
| logand     | 50         | `(logand a b)`
| &          | 50         | `(logand a b)`
| logxor     | 40         | `(logxor a b)`
| ^          | 40         | `(logxor a b)`
| logior     | 30         | `(logior a b)`
| &#92;&#124;| 30         | `(logior a b)`
| and        | 20         | `(and a b)`
| or         | 10         | `(or a b)`
