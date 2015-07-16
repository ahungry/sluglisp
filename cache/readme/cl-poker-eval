cl-poker-eval
=============

Pure common lisp 7 card poker hand evaluation.  Evaluation based on pokersource.  

Loadable via quicklisp.
```cl
* (ql:quickload "cl-poker-eval")
```

cl-poker-eval uses the following enumeration for card values: 
```
2c-Ac = 0-12  
2d-Ad = 13-25  
2h-Ah = 26-38  
2s-As = 39-51  
```

You can use eval-hand-var to evaluate hands with 5 to 7 cards, or eval-hand-7 to evaluate a 7 card hand only. For example, to get the value of 7 cards in a list you could do the following:  

```cl
(defvar *hand* '(1 2 3 4 5 6 7)) ;this would be the hand 3c 4c 5c 6c 7c 8c 9c  
(apply #'cl-poker-eval:eval-hand-var *hand*)  
```

You could also call eval-hand-* functions with the cards values you want to evaluate, like so:  

```cl
(cl-poker-eval:eval-hand-7 1 2 3 4 5 6 7)  
(cl-poker-eval:eval-hand-var 3 4 5 6 7)  
```

Values returned by eval-hand-* integers representing the strength of a hand.  High values are stronger hands.
