# About

cl-speedy-queue is a portable, non-consing, optimized queue implementation. It was originally
written by [Adlai Chandrasekhar](http://github.com/adlai) for use in
[ChanL](http://github.com/zkat/chanl).

# API

*[function]* `make-queue size`

  Creates a new queue of SIZE.

*[function]* `enqueue object queue`

  Enqueues OBJECT in QUEUE.
  
*[function]* `dequeue queue`

  Dequeues QUEUE.
  
*[function]* `queue-count queue`

  Returns the current size of QUEUE.
  
*[function]* `queue-length queue`

  Returns the maximum size of QUEUE.
  
*[function]* `queue-peek queue`

  Returns the next item that would be dequeued without dequeueing it.

*[function]* `queue-full-p queue`

  Returns NIL if more items can be enqueued.
  
*[function]* `queue-empty-p queue`

  Returns NIL if there are still items in the queue.
