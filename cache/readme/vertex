# VerTeX

[![Build Status](https://travis-ci.org/CommonDoc/vertex.svg?branch=master)](https://travis-ci.org/CommonDoc/vertex)

A markup language with TeX syntax.

# Syntax

## Basic

VerTeX syntax, as the name implies, is basically TeX syntax. Blocks look like
this:

* `\some-tag`
* `\b{bold text}`
* `\link[uri='https://www.google.com/']{Google}`

The syntax, as a sort of regular expression, is like this:

```
\<tag name>([(<attribute>=<value>)*])?({<body>})?
```

## Markup

### Paragraphs

Paragraphs are delimited by double newlines.

```tex
A paragraph.

Another paragraph with \b{bold text}.

A third paragraph.
```

### `b`

Bold text.

```tex
This is \b{bold text}.
```

### `i`

Italicized text.

```tex
This text is in \i{italics}.
```

### `u`

Underlined text.

```tex
This text is \u{underlined}.
```

### `strike`

Struck-through text.

```tex
This text is \strike{struck through}.
```

### `sup` and `sub`

Superscript and subscript.

```tex
The value of the vacuum permittivity, ε\sub{0}, is 8.8x10\sup{-12}.
```

## Code

### `c`

Inline code.

```tex
The function \c{find} takes as arguments...
```

### `code`

A block of code.

```tex
\code[lang='lisp']{
  (let ((x 1))
    (incf x))
}
```

## Quotes

### `q`

An inline quote.

```tex

```

### `quote`

A block quote.

```tex

```

## Links and References

### `ref`

A reference to a section of the document, or to another document.

```tex
See section \ref[sec=search]{Search}.

For a more thorough discussion, see \ref[doc=aima, sec=search]{the AIMA chapter}
on search algorithms.
```

### `link`

A link to a URI.

```tex
Visit \link[uri='https://www.google.com/']{Google}.
```

## Lists

### `list`

An unordered list.

```tex
Ingredients:

\list{
  \item{One egg}
  \item{One tablespoon of olive oil}
  \item{Grated cheese}
}
```

Produces:

* One egg
* One tablespoon of olive oil
* Grated cheese

### `enum`

An ordered list.

```tex
Recipe for eudoxia's patent-pending microwave eggs:

\enum{
  \item{Pour the olive oil into the bowl}
  \item{Crack the egg into it}
  \item{Put the cheese on top}
  \item{Microwave for 45 seconds}
}
```

Produces:

1. Pour the olive oil into the bowl
2. Crack the egg into it
3. Put the cheese on top
4. Microwave for 45 seconds

### `deflist`

A definition list.

```tex
\deflist{
  \term{Sum Rule}
  \def{If two tasks can be performed in m and n ways, respectively, then
  there are m+n ways of doing \b{either}.}

  \term{Product Rule}
  \def{If two sequential tasks can be performed in m and n ways,
  respectively, there are m*n ways of performing the sequence.}
}
```

## Images and Figures

## Tables

### `table`, `row`, `cell`

Exactly what you would expect.

```tex
\table{
  \row{
    \cell{} \cell{\b{Peach}} \cell{\b{Egg}}
  }
  \row{
    \cell{\i{Fat}} \cell{0.25g} \cell{10.6g}
  }
  \row{
    \cell{\i{Protein}} \cell{0.91g} \cell{12.6g}
  }
}
```

Produces:

|             | **Peach** | **Egg** |
| ----------- | --------- | ------- |
| *Fat*       | 0.25g     | 10.6g   |
| *Protein*   | 0.91g     | 12.6g   |

## Structure

### `section`

Defines a section.

```tex
\section[title=The Reader]{
  ... For other stuff see the chapter on \ref[sec=compiler]{Compilation}.
}

\section[title=The Compiler, ref=compiler]{
  ... A compiler is basically ...
}
```

# License

Copyright (c) 2014-2015 Fernando Borretti

Licensed under the MIT License.
