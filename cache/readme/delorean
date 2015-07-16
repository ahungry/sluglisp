# delorean

A simple set of macros for mocking time

## Usage

    (deftest delorean ()
      (let ((point-in-time (local-time:now)))
        (with-frozen-clock point-in-time
          (sleep 2)
          (is (local-time:timestamp= (local-time:now) point-in-time))))
    
    
      (let ((same-time-tomorrow (local-time:timestamp+ (local-time:now) 1 :day)))
        (with-shifted-clock same-time-tomorrow
          (sleep 2)
          (is (local-time:timestamp= (local-time:now)
               (local-time:timestamp-minimize-part
                (local-time:timestamp+ same-time-tomorrow 2 :sec) :nsec)))))
    
      
      (let ((now (local-time:now)))
        (with-scaled-clock 2
          (sleep 2)
          (is (local-time:timestamp= (local-time:now)
                (local-time:timestamp-minimize-part
                  (local-time:timestamp+ now 4 :sec) :nsec))))))

The respective clocks can be nested arbitrarily inside one-another.

## Development

Development of delorean is hosted on github at https://github.com/cddr/delorean.

To my knowledge, there is currently 1 user (i.e. me) so I'm just hacking directly
on master. If you start to find it useful, let me know and we can formalize the
release process a little but I don't foresee lots of changes. It's a pretty simple
solution to a small problem.

Check out the Makefile for the details of how to build and test the software. It's
shamelessly copied from cffi (Luís Oliveira is awesome). It also includes a target
to build documentation using atdoc (David Lichteblau is also awesome).

## Bugs?

What bugs?  No really, if you find a bug, let me know by raising a github issue.
