About Lambda-Fiddle
-------------------
Lambda-fiddle is a collection of utilities to process lambda-lists. Mostly useful in contexts where you want a macro that uses lambda-lists in some fashion but need more precise processing.

How To
------
Included are --for each standard lambda-list part-- a `remove-*-part` and `*-lambda-var/s` function that remove the specified part or extract the associated variable/s, respectively. These are all based on the more generale `collect-for-keyword` and `exclude-for-keyword` functions that will scan the given lambda-list for the keyword and collect accordingly. The generalised functions may also be useful if you plan to include your own non-standard keywords.

    (lambda-fiddle:remove-aux-part '(foo bar &key baz &aux (something else) altogether))
    (lambda-fiddle:aux-lambda-vars '(foo bar &key baz &aux (something else) altogether))

Additionally, there is `split-lambda-list` and its macro partner `with-destructured-lambda-list` that split the lambda-list into all its parts and return/bind them accordingly, allowing for easy access.

    (lambda-fiddle:split-lambda-list '(&whole wide world &key roles))
    (lambda-fiddle:with-destructured-lambda-list (:required req) '(foo bar &optional baz)
      req)

