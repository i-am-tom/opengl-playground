# Behaviours

_Elements of the past and the future combining to make something not quite as
good as either._

* [`purescript-behaviours`](https://github.com/paf31/purescript-behaviors) is
  great, and I wanted it to exist in Haskell.

* [Push-Pull Reactive Functional Programming](http://conal.net/papers/push-pull-frp/push-pull-frp.pdf)
  is a fascinating paper, so I wanted to try to implement the bits that I
  needed.

Semantics are tricky, so I took Phil's `Event` type as a starting point and
tried to build out from there. The result is, unsurprisingly, a slightly worse
implementation of Phil's ideas. This code is hence distributed under Phil's
licence.

# Events

Event streams are modelled as a list of pairs of a timestamp and a value. We
assume these values are given in ascending order of timestamp. Event streams
are thus a sort of zippy applicative: every value from the first stream is
paired with the most "recent" value of the second stream, and vice versa.

# Behaviours

Behaviours are modelled as a function from a timestamp to a value. In other
words, events specify changes of value at discrete points in time, but
behaviours specify (and vary) values as functions of time. This means we can do
clever things like model animationos irrespective of frame rate: we sample the
behaviour on every frame, and its value is computed based on the time.
