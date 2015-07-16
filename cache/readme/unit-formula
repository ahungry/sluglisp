# Overview

This is a library for unit conversions and defining formulas with automated unit consistency
checking and conversions. This is similar to [http://www.cs.utexas.edu/users/novak/units95.html],
but written with ease of implementation over optimization. In other words, I understand how mine
works better. Most of the actual unit data is taken from that program, which is why this is under
GPLv2.

This is not yet well tested. Any comments are welcome.

# Dependencies

- iterate
- alexandria

# Usage
## Unit definition language

Units are described by s-expression. Units are identified by symbols, but interpreted by symbol
name, so the package is irrelevant. There is significant number of units already defined in
unit-data.lisp, check there or keys of `units-formula::*units*` hashtable for a list. Units can be
constructed from those by use of `*` `/` `expt` `sqrt` operators. In a list with a unit name as
first element `*` is implied. Numbers might be included in unit definition, they will be combined
into contant factor.

## Reference

Function `reduce-unit unit-spec` turns the list in above format to an unit object, which can be used
in any place where unit definition can be used, to avoid repeated reduction of unit definition.

Function `convert-unit unit-from unit-to` takes two unit objects and returns a conversion factor
between them, or :incorrect-conversion if the units do not match.

Example:

`CL-USER> (unit-formulas:convert-unit '(/ parsec fortnight) '(/ km second))
2.5487764e7`

numbers can be included, as mentioned above, to convert between values, rather than to obtain
conversion factor:

`CL-USER> (unit-formulas:convert-unit '(5 kg) 'pound)
11.023113`

Function `same-unit-p unit1 unit2` takes two units and returns true if they are compatible. If key
argument `:factor` is true, then equality between constant factors is also checked.

Function `dimensionless-p unit` returns t if the unit is dimensionless (ie. only the constant factor
is relevant). It's value can be retrieved with `(convert-unit dimensionless-value nil)`

Macro `define-operators list-of-operators kind-keyword` allows definition of operators allowed in
formulas. Right now only :agree and :dimensionless kinds are present, which require all arguments to
be the same unit or dimensionless respectively.

Macro `defformula name (&rest in-spec) formula-expression` defines a formula. This creates a
function named `name`, which takes a &rest argument forming an association list of form (name value
unit) or (name unit-with-value). Argument in-spec is a list of form (name unit) or (name unit
value). The second form creates a named constant which will be folded into the formula. Note that
this has to literal number because this is folded at macroexpansion stage. Units in in-spec would in
most cases be base units, which have synonym symbols with the name of what it is an unit of.

Formula-expression consists of operators defined in `units-formula::*operators*` hash table, which
must have directly corresponding functions defined. Other allowed expressions are: symbol, naming
first a binding defined in in-spec, which will be replaced either by function argument or constant
value, if provided, a literal constant, either a number, an unit name, or (number unit-definition).

Created function returns an unit object, which can be converted to value in desired units with
`convert-unit`, or queried directly with `query-unit`.

Example:

    CL-USER> (unit-formulas:defformula K-np 
                                       ((effective-mass mass)
                                        (delta-e energy)
                                        (h-bar (/ (m m kg) s) #.(/ 6.62d-34 (* 2 pi)))
                                       (f electric-field))
                   (/ (/ (* 4 (sqrt (* 2 effective-mass (expt delta-e 3))))
                         (abs f))
    		      (* 3 elementary-charge h-bar)))
    K-NP
    CL-USER> (k-np '(effective-mass 0.2 electron-mass) '(delta-e 0.8 eV) '(f 0.09 (/ V (nano m))))
    #<UNIT-FORMULAS::UNIT 24.309902549224955d0 >

Function `query-unit unit` returns a property list with unit value and exponents of base SI units
forming an unit.

Function `identify-unit unit` tries to find a quantity with the same units, and if found returns a
keyword naming it.