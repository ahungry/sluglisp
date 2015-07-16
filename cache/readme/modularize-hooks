About Modularize-Hooks
----------------------
This is a simple extension to [Modularize](https://github.com/Shinmera/modularize) that allows modules to define and trigger hooks, which other modules can... well, hook on to. While such a system is very easy to create for your own projects, it's a good idea to have a standard form of doing it that doesn't interfere in any other way. 

How To
------
In order to define hooks, you need to have a module. See [Modularize](https://github.com/Shinmera/modularize) on how to get that set up. If the modularize-hooks system has already been loaded prior to your module definition, everything should be ready and set up. Otherwise, you may call `HOOKIFY` on your module to initialise it. Creating hooks is similar to creating generic functions:

```
(define-hook documentation-finished (project)
  "Hook called whenever documentation for a project is finished.")
```

This defines a hook with a single argument, the project. In order to latch on to this hook, you may define a trigger. It is not strictly necessary to be within a module context to define triggers, but it is recommended in order to keep things tidy.

```
(define-trigger other-module:documentation-finished (project)
  (format T "~&Finished project documentation for ~a" project))
```

If you want to have multiple triggers on the same hook within the same package, you need to specify some kind of identifier to distinguish it from others.

```
(define-trigger (other-module:documentation-finished extra) ()
  (format T "~&ゆっくりしていってね！！！"))
```

Note that in this second example we did not provide any arguments to the trigger. Triggers may either accept no arguments if they have no use for them, or they need to match the arguments list of the hook. A simple test to assert this is done on trigger definition. Actually triggering the hook with its trigger is merely a question of calling `TRIGGER`:

```
(trigger 'other-module:documentation-finished :modularize-hooks)
```

