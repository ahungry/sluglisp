# clods-export

It is sometimes necessary to offer users of e.g. a web service the
option to export their data from your application to the outside
world.  Often, a good format would be a spreadsheet document, as it
offers a simple way of displaying and editing tabular data, and people
have over the years become quite familiar in misusing Excel for a
variety of things Excel was not designed for.

clods-export helps you by writing out OpenDocument Spreadsheet files.
It does not try to implement functionality needed by proper
spreadsheet applications and it does not support reading in ODS
formatted data, but it tries to make creation of ODS files
straightforward and easy.

For a quick start, look at [example.lisp](https://github.com/jlahd/clods-export/blob/master/example.lisp).
A short API reference is given below, but to fully understand how all
parameters work I recommend studying the [OpenDocument XML format specification](http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html)
as well.

Caveat! Different software (Excel, LibreOffice, OpenOffice) handle ODS
data in a different manner, and are a bit incompatible.  So, you
should check your generated data on all of the applicable programs in
order to see that all formatting goes through as you intended.

## Document structure

An ODS document consists of three parts: Definitions for fonts,
definitions for styles, and the actual data content. These parts must
be specified, in this order, to clods-export as well.

Start by wrapping your export functionality inside a `with-spreadsheet`
form.  The name of the generated ODS file is given as an argument,
along with metadata (name of the application generating the document
and the human creator of the data).  Inside the spreadsheet, you then
define fonts, styles and content:

```cl
(clods:with-spreadsheet ("my.ods" :generator "My app" :creator "Me")
  (clods:using-fonts ()
    ...)
  (clods:using-styles (:locale ...)
    ...)
  (clods:with-body ()
    (clods:with-table ("Sheet one")
      ...)
    (clods:with-table ("Sheet two")
      ...)
    ...))
```

## Specifying fonts

Fonts are specified using the `clods:font` function inside a
`clods:using-fonts` form. `clods:font` takes the name of the font
definition as its first argument, and a number of optional key
arguments describing the font properties:

* `:family` (string), for example as "Arial"
* `:family-generic` (keyword), from clods:*font-generic-families*
* `:size` (string), length value such as "12pt" or "0.8cm"
* `:style` (keyword), from clods:*font-styles*
* `:weight` (keyword), from clods:*font-weights*
* `:variant` (keyword), from clods:*font-variants*
* `:stretch` (keyword), from clods:*font-stretches*
* `:adornments` (string).

Note that certain font details can be specified along with text
properties when defining cell styles, as well.

Excel understands the name of the font definition as a synonym for the
font family.  That is, if you want to be Excel compatible, you must
match the `font`'s name argument with the `:family` argument:

```cl
;; works on LibreOffice and OpenOffice but not on Excel:
(clods:font "normal" :family "Arial")
;; works on all three:
(clods:font "Arial" :family "Arial")
```

## Specifying styles

ODS defines formatting on several levels. On the lowest level, there
are data styles (number styles) that specify how data is formatted
into strings to be displayed.  Then, cell styles specify formatting
inside a single cell.  Column and row styles define the width/height
of the column/row as well as the default cell style to be applied.

Styles are defined hierarchically, so that styles can inherit
properties from other styles.  However, this inheritance fails
spectacularly on LibreOffice and OpenOffice (but does seem to work on
Excel), so it is probably a good idea to define every style from the
bottom up.

Both data styles and cell styles also contain text properties that
define visual aspects of the displayed text.

### Locales

Locale, as defined by `clods-export`, is a simple object that contains
the following four slots:

* `country` (string) the associated ISO 3166 country code
* `grouping-separator` (base-char) the character inserted between number groups
* `grouping-count` (integer) length of a single number group
* `decimal-separator` (base-char) the character between integer and decimal parts of a real number.

Locales can be created with the function `clods:make-locale`.  For
example, the Finnish locale, where a large decimal number is written
as "1 234 567,89", would be defined as follows:

```cl
(clods:make-locale "FI" #\space 3 #\,)
```

### Text properties

Text property definitions are lists containing pairs of keywords and
values.  The set of supported keywords is listed in
`clods:*text-properties*` and they map directly to those defined [in the OpenDocument specification](http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html#__RefHeading__1416402_253892949).

### Number formatting

Numbers can be displayed in three different representations: standard
numbers, scientific numbers and fractions.  The representation to be
used is deduced from the arguments in the number format specification,
which is a list of keyword-value pairs.  The following keywords are
supported:

* `:min-integer-digits` specifies the minimum number of digits in the
  integer part of the number.  Supported by all number
  representations.
* `:decimal-places` specifies the minimum number of decimal digits
  after the decimal separator.  Supported by standard and scientific
  numbers.
* `:decimal-replacement` specifies the string to be added (instead of
  zeros) as the decimal part of an integer number, if
  `:decimal-places` is specified as well.
* `:display-factor` scales down the number for displaying.  Supported
  by standard numbers only.
* `:number-grouping` groups the integer part of the number according
  to the locale.  Supported by standard numbers only.
* `:min-exponent-digits` specifies the minimum number of exponent
  digits to be shown.  Supported by scientific numbers only; adding
  this flag forces scientific representation.
* `:denominator-value` forces the use of a specific denominator in the
  fraction.  Supported by fractions only; adding this flag forces
  fractional representation.
* `:min-denominator-digits` specifies the minimum number of digits on
  the denominator of the fractional number.  Supported by fractions
  only; adding this flag forces fractional representation.
* `:min-numerator-digits` specifies the minimum number of digits on
  the numerator of the fractional number.  Supported by fractions
  only; adding this flag forces fractional representation.

Note that LibreOffice and OpenOffice require quite complete number
specifications in order to display numbers correctly.  Thus, you
should specify things like `:min-integer-digits 1` even though they
seem superfluous, if you wish these applications to show your data
correctly.

### Data styles

There are several different data formatting types. In addition to the
type-specific arguments described below, they all accept the keyword
arguments `:locale` (of type `locale`) and `:text-properties` (a list
that contains text property definitions).

* `(number-boolean-style name true false &key prefix suffix)` formats
  data that must be one of the keywords `:true` and `:false` into the
  textual representations given on the `true` and `false` (string)
  arguments.  An optional prefix or suffix can be added.
* `(number-time-style name format)` formats a `local-time:timestamp`
  into a textual representation given in `format`.  `format` must be a
  list that contains strings (which will be formatted as such) and
  keywords from the following set: `:long-hours`, `:short-hours`,
  `:long-minutes`, `:short-minutes`, `:long-seconds`, `:short-seconds`
  and `:am-pm`.  If the `:am-pm` keyword is present, hours will be
  formatted in 12-hour format, otherwise in 24-hour format.  The
  `long` versions will format the data in two digits, `short` versions
  using one or two digits.
* `(number-date-style name format)` formats a `local-time:timestamp`
  in a similar manner to the `number-time-style`.  All the formatting
  directives specified above work here as well, with the addition of
  the following keywords: `:long-day`, `:short-day`, `:long-month`,
  `:short-month`, `:long-year`, `:short-year`, `:long-era`,
  `:short-era`, `:short-day-of-week`, `:long-day-of-week`,
  `:short-week-of-year`, `:long-week-of-year`, `:short-quarter` and
  `:long-quarter`.  However, `clods-export` does not currently support
  formatting era, day-of-week and week-of-year.
* `(number-number-style name format &key prefix suffix)` is the main
  number formatting facility.  The `format` argument is a list as
  defined above in the Number formatting section.  Additionally,
  arbitrary strings given in the `prefix` and `suffix` arguments can
  be prefixed or appended to the formatting result.
* `(number-percentage-style name format &key prefix suffix)` formats a
  number as a percentage value.  In essence, this means that the value
  is multiplied by 100 before being displayed.  Also, `suffix`
  defaults to the string " %".
* `(number-currency-style name format)` formats a number as a monetary
  value.  The format here is a list of keyword-value pairs.  The
  keyword `:number` precedes a list that specifies number formatting
  as in the previous number styles, and the keywords `:symbol` and
  `:text` should be followed by strings that are printed verbatim.
  The difference between `:symbol` and `:text` values is semantic;
  visually, they produce similar results.
* `(number-text-style name &key prefix suffix)` simply formats a
  string input by preceding it with the optional prefix and appending
  the optional suffix.

### Cell styles

For a cell style, you can define text properties, and a set of other
formatting keyword arguments:

`(clods:cell-style name parent-style text-properties &key ...)`

Cell styles support inheritance, so you can build your styles on top
of each other by using the parent-style argument.  Note, though, that
not all applications obey the inheritance.

The available keyword arguments are:

* `:horizontal-align` (one of `:start`, `:center`, `:end`,  `:justify`, `:left`, `:right`)
* `:vertical-align` (one of `:top`, `:middle`, `:bottom`, `:automatic`)
* `:text-align-source` (one of `:fix`, `:value-type`)
* `:background` (`:transparent` or a string color definition "#xxxxxx")
* `:border`, `:border-left`, `:border-top`, `:border-right`,
  `:border-bottom` specify a list of three elements: (*width* *style*
  *color*), where *width* is one of `:auto`, `:normal`, `:bold`,
  `:thin`, `:medium`, `:thick`; *style* is one of `:none` `:solid`
  `:dotted` `:dash` `:long-dash` `:dot-dash` `:dot-dot-dash` `:wave`,
  and *color* is a string color definition "#xxxxxx".  Specifying
  `:border` applies the same border to all edges.
* `:wrap` takes a generalized boolean defining if the cell's contents wrap or not.

### Row styles

Row styles are defined as follows:

`(clods:row-style name parent-style &key ...)`

The available keyword arguments are:

* `:height` (string) height of the row, as a length value (e.g. "16pt" or "12mm")
* `:min-height` (string) as above
* `:use-optimal-height` (generalized boolean) allow the application to
  automatically set the row's height according to content
* `:background` (`:transparent` or a string color definition "#xxxxxx")

### Column styles

Column styles are defined as follows:

`(clods:column-style name parent-style &key ...)`

The available keyword arguments are:

* `:width` (string) width of the row, as a length value (e.g. "90pt" or "5.5cm")
* `:rel-width` (string) as above
* `:use-optimal-width` (generalized boolean) allow the application to
  automatically set the column's width according to content

### Table styles

Table styles are included for the sake of completeness.

`(clods:table-style name parent-style &key ...)`

The available keyword arguments are:

* `:width`
* `:rel-width`
* `:align`
* `:background`.

## Table content

Each worksheet on the document are defined inside a `clods:with-table`
form.  The sheet's name, usually shown on a tab at the bottom of the
spreadsheet application's view, is given as an argument.  Inside the
`with-table` form first the table's columns are defined, followed by
the rows containing the actual data in cells:

```cl
(clods:with-table ("Table name")
  (clods:with-header-columns ()
    (clods:column ...)
    ...)
  (clods:with-header-rows ()
    (clods:with-row (...)
      (clods:cell ...)
      ...)
    ...)
  (clods:with-row (...)
    (clods:cell ...)
    ...))
```

The semantic grouping of certain columns and rows into the header
columns/rows groups is optional and has no visual effect on the table.

### Defining columns

`(clods:column &key repeat style visibility cell-style)`

* If the `:repeat` argument is specified, the column is repeated the
  specified number of times, making it easy to define a table with many
  similarly formatted columns.
* `:style` refers to a previously defined `column-style`.
* `:visibility` is one of `:visible`, `:collapse` and `:filter`.
* `:cell-style` refers to a previously defined `cell-style`, and defines
  the default style to be applied for all cells in this column.

### Defining rows

`(clods:with-row (&key repeat style visibility cell-style) ...)`

The keyword arguments are similar to those given to `clods:column`.

### Defining cells

`(clods:cell content &key style formula span-columns span-rows link)`

Cells contain the actual data on the table.  The content of each cell
is given in the `content` argument that may be `nil` if the cell is
empty.  The keyword arguments are as follows:

* `:style` refers to a previously defined `cell-style`.
* `:formula` is a string that contains a formula for the cell.  Formulas
  are not understood or processed by `clods-export`; they are simply
  written to the document as-is.
* `:span-columns` and `:span-rows` can be used to make the cell span
  several adjacent cells in either direction.  Horizontally spanned
  cells (`:span-columns`) are automatically marked as covered, but
  vertically covered cells must be handled by the application (see
  `clods:covered-cell` below).
* If `:link` is specified, the cell is made into a hyperlink with the
  `link` argument as the target.

If the cell contains a `:style` argument, that style is used for
formatting.  Otherwise, if the current row has specified a
`:cell-style`, that is used.  If not, but the current column specifies
a `:cell-style`, it takes effect.  Otherwise, only string content is
supported.

In addition to the data style specified by the active cell style, the
formatting of the cell's content depends on the type of the `content`
argument.

* `null` content means an empty cell.
* `real` numbers are written using a number formatter.  The active data style
  must be one of `number-number-style`, `number-currency-style` and `number-percentage-style`.
* `local-time:timestamp` specify dates and times.  The active data style
  must be one of `number-time-style` and `number-date-style`.
* `keyword`, one of `:true` and `:false`, means boolean content.  The
  active data style must be a `number-boolean-style`.
* `string` content is written out as-is, regardless of the active data style.
  However, if the active data style is `number-text-style`, the possible
  prefix and suffix information are used in the formatting.

`(clods:cells &rest content)`

For convenience, a set of adjacent cells on the same row requiring no
special formatting can be written out in a single function call to
`clods:cells`.

`(clods:covered-cell &optional n)`

When a cell has a `:span-rows` argument larger than 1, the adjacent
cells on the following rows in the same column (that is, those cells
covered by the spanning cell) have to be marked as covered.
`clods-export` does not take care of this; it is left to the
application.

## Contact information

If you have a bug to report, or an enhancement to suggest, you can
reach me at [jussi@lahdenniemi.fi](mailto:jussi@lahdenniemi.fi).
