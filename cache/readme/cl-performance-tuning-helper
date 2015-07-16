A simple performance tuning helper tool box for Common Lisp
====================================

## APIs:
### Function `CLOAD`:
load a file after compile.

e.g. `(pth:cload "src")` for compile and load "`src.lisp`".

### Function `ASMOUT`:
save `DISASSEMBLE` result for given function into a file. You can specify saving filename optionally.

When a function is given as an symbol s.t. `'SOME-FUNCTION`, the default filename is a lowercase version of the symbol name with "`.asm`" suffix.

e.g. `(pth:asmout 'SOME-PACKAGE:SOME-FUNCTION)`

saves `DISASSEMBLE` result for `#'SOME-PACKAGE:SOME-FUNCTION` into "`some-function.asm`".

When a function is given as a lambda expression or a function itself (i.e. `#'SOME-FUNCTION` or `(SYMBOL-FUNCTION 'SOME-FUNCTION)`), you *must* specify filename explicitly.

e.g. `(pth:asmout (lambda (x y) (+ x y)) "binary-add.asm")`

saves `DISASSEMBLE` result of a given lambda expression into "`binary-add.asm`". In this case, suffix ("`.asm`") is not appended automatically.

### Macro `PERFORMANCE`:
simple performance checker.

e.g. 1 `(pth:performance 100 debugger-p (some-function args...))`

tries to eval `SOME-FUNCTION` 100 times with specified args.

After all, displays execution information by `TIME` macro.

`PTH:PERFORMANCE` returns t if all eval has been done successfully.

When a condition has been signaled;

- when *debugger-p* is `nil`:  
  `PTH:PERFORMANCE` returns `nil`.
- when *debugger-p* is non-`nil`:  
  invoke debugger for any kind of conditions.

e.g. 2

target function also able to access current-num-repeats.

        (let ((max 5))
          (pth:performance (count max) nil
            (format t "~&~d of ~d~%" count max) ))
        ;;; performance test for FORMAT 5 times
        ;;;   do (FORMAT T "~&~d of ~d~%" COUNT MAX)
        0 of 5
        1 of 5
        2 of 5
        3 of 5
        4 of 5
        (and implementation dependant TIME macro result)
        ; => T

## LICENSE:
under MIT license.

