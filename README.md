lens-talk-clojure
=================

This is the code for the clojure version of my talk, _Lens from the ground up_.

The slides are available at <https://speakerdeck.com/markhibberd/lens-from-the-ground-up-in-clojure>.

The haskell version is available at <https://github.com/markhibberd/lens-talk>.

To follow along, I basically worked through the code and examples in [script.org](https://raw.githubusercontent.com/markhibberd/lens-talk-clojure/master/lens/script.org).

The most interesting pieces of code are:
 - The basic _van Laarhoven lens_ implementation: [core.clj](https://github.com/markhibberd/lens-talk-clojure/blob/master/lens/src/lens/core.clj)
 - The JavaScript refactoring tool (in Haskell): [Js.hs](https://github.com/markhibberd/lens-talk/blob/master/src/Js.hs)
