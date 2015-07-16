# trivial-channels

This is a very, very trivial implementation of channels (and a
queue).  I find myself using it in a few places where very trivial
message passing is needed and a more complex, robust solution would be
overkill.

```lisp
(let ((channel (make-channel)))
  (sendmsg channel 'anything)
  (recvmsg channel)) ;; => ANYTHING
```

## API

* `(make-channel)`: Make a channel
* `(sendmsg CHANNEL OBJECT)`: Send `OBJECT` on `CHANNEL`
* `(recvmsg CHANNEL &optional TIMEOUT)`: Receive on `CHANNEL`,
  optionally timing out after `TIMEOUT` seconds
* `(getmsg CHANNEL)`: Get a message if available, or `NIL`
* `(hasmsg CHANNEL)`: Whether the channel has a message.  No guarantee
  it will still have one after the call, if there are multiple
  receivers sharing a channel.

Notably, `recvmsg` supports a timeout.  These functions properly lock
and it's safe to share a channel between threads (that being the
entire purpose).

## Usage

While trivial-channels should be simple enough you can adapt it to
many usage patterns, for simple bi-directional message passing I have
found it easiest to simply pass a message with a return-channel
included:

```lisp
;;; Sender:
(defvar *global-listener* (make-channel))
(defvar *done* nil)

(let* ((return-channel (make-channel))
       (msg (cons 'value return-channel)))
  (sendmsg *global-listener* msg)
  (recvmsg return-channel))

;;; Meanwhile, in another thread:
(loop until *done* do
  (let ((msg (recvmsg *global-listener*)))
    (let ((value (car msg))
          (channel (cdr msg)))
      ;; insert useful things here
      (sendmsg channel ...))))
```

Of course, you needn't create a new return channel every time, either,
if you are worried about consing, but this is an easy way to pass
functions to a specific thread, implement actors, etc.

## Queues

The queue used to implement this is also available via the package
`:trivial-channels.queue`, since it's sometimes handy to have a
trivial queue, too.

Queues do **not** lock.

Queues are implemented with conses.

* `(make-queue)`: Make a queue
* `(queue-head Q)`: Head of the queue
* `(queue-tail Q)`: Tail of the queue
* `(queue-add-cons Q CONS)`: `CONS` becomes the tail of the queue; its
  CDR may be destructively modified
* `(queue-add Q ITEM)`: Add `ITEM` to the tail of the queue
* `(queue-push Q ITEM)`: Push `ITEM` onto the front of the queue
* `(queue-pop Q)`: Pop and return and item from the queue, or NIL
* `(queue-pop-cons Q)`: Pop and return the cons from the queue, or NIL
* `(queue-has-item-p Q)`: If there are items in the queue, or NIL
* `(queue-peek Q)`: The front item in the queue, or NIL
* `(queue-pop-to Q1 Q2)`: Pop an item from `Q1` and add it to `Q2`
  without consing
* `(queue-prepend-to Q1 Q2)`: Remove all items from `Q1` and prepend
  them to `Q2` without consing; no value returned

All add or remove type operations return the item (or cons) being
handled, unless otherwise noted.
