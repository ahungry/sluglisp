# Dweet

This is a [dweet.io](https://dweet.io) client for Common Lisp.

Careful, boredom begets software of questionable value.

# Example

Dweet machine information to `lispmachines`:

```lisp
(dweet:with-thing "lispmachines"
  (dweet:post :instance (machine-instance)
              :type (machine-type)
              :version (machine-version)))
```

The first parameter to `dweet:post` can be the thing's name:

```lisp
(dweet:post "luckynumbers" :n (random 100))
```

Fetch latest dweet:

```lisp
(dweet:latest "lispmachines")

;; ==> ("thing" "lispmachines" "created" "2014-10-25T00:00:29.239Z" "content"
;;      ("instance" "sneeze.adeht.org" "type" "X86-64" "version"
;;       "Intel(R) Core(TM) i5-4460  CPU @ 3.20GHz"))

```

Fetch "all" dweets (within dweet.io limitations):

```lisp
(dweet:all "luckynumbers")

;; ==> #(("thing" "luckynumbers" "created" "2014-10-25T00:25:33.099Z" "content" ("n" 68))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:31.965Z" "content" ("n" 37))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:31.038Z" "content" ("n" 99))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:29.288Z" "content" ("n" 43))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:27.884Z" "content" ("n" 58))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:26.522Z" "content" ("n" 97))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:25.189Z" "content" ("n" 5))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:24.071Z" "content" ("n" 95))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:25:22.732Z" "content" ("n" 44))
;;       ("thing" "luckynumbers" "created" "2014-10-25T00:24:25.401Z" "content" ("n" 92)))
```

Listen to dweets as they come:

```lisp
(dweet:listen (lambda (dweet)
                (format t "~A~%" dweet))
              "lispmachines")
```

# To Do

Locking and alerts and stuff...

# License

MIT
