# time-interval

Cyrus Harmon

time-interval is a common lisp library for flexibly encoding time
intervals. It relies heavily on the local-time library for the
representation of particular points in time, or as local-time calls
them, timestamps.

The primary motivation for time-interval is to be able to represent
periods of time such as "6 months from July 1, 2008". The idea here
isn't to represent the start and dates (or times) explicitly, but
rather to flexibly represent the interval. The difficulty arises from
the fact that a length of time such as "6 months" can't be uniquely
represented as a number of days, hours, minutes, etc... Depending on
what the starting point is, the number of days (hours, etc...) in a
6-month interval will change. Therefore, we need a way of representing
the time intervals and for computing a new exact time given a starting
time and a time interval.

# Required Libraries:

cl-ppcre, local-time

# Classes:

## time-interval

The time-interval class is used for representing a given interval of
time, such as 6 months, 2 days, 4 hours and 3 seconds. Each of the
components can independenly be positive or negative, so we can have,
for instance, an interval that represents 1 hour less than two days,
by doing:

    (make-instance 'time-interval :hours -1 :days 2)

or:

    (time-interval :hours -1 :days 2)

