Message OO
==========

This package gives you the great opportunity of real object-oriented
programming.

For example, you may write

    (use-package :message-oo)
    
    (defmessage list (:add item) (append list (list item)))
    (defmessage list (:map func) (mapcar func list))
    (defmessage list (:reduce func) (reduce func list))
    (defmessage number (:+ number2 &rest numbers) 
       (apply '+ number number2 numbers))

    (@ nil
       (:add 1)
       (:add 2)
       (:add 3)
       (:map (@ '(:+ 3) :fn))
       (reduce #'+)) => 15

You may think of last command as nil.add(1).add(2).add(3).map(..).reduce(...)

Every next step is applied to the result of previous as in UNIX shell pipeline.

Enjoy!