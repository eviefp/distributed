# distributed

`distributed` is a runtime system which allows running code on multiple
(networked) nodes, connecting nodes and passing data across nodes.

The main idea is to use `Star` (Kleisli arrows such as `a -> m b`) as the "unit"
of computation and compose them in some sort of graph.

There are a few main parts:
- an API which describes this "graph"
- a runtime system which executes this graph
- the ability to serialize and send data across the wire


Flow: A ---- B ---- C ---- D

N, M

Strategy Cycle:
A ---- B ---- C ---- D
M      N      M      N


## Plan for next stream: Runtime System YAY
- configuration:
    - run in listen mode (do not try to connect to other nodes)
    - run in connect mode (connect to other nodes, with specified addresses)
    - node identifier
- provide simple API to send messages across nodes
    - messages should be things like "print something to console" or "PING/PONG"s


### Old ideas

# 1. Actor systems
- you create actors
- actors can
    - receive messages
    - change their state
    - send messages to other actors

We could model this using actors and just send messages between them.
+ relatively well-understood model with good existing models & documentation
- if any actor can call any actor, this can quickly become spaghetti code

# 2. Star / kleisli functions
- you define functions `a -> m b`
- whenever we want to compose `a -> m b` and `b -> m c`, we cannot use `>=>`
    because we want to keep the two functions SEPARATE

```haskell
f :: a -> b
g :: b -> c

---------

h = g . f

-- you cannot pattern match or do anything to `h` such that you obtain g and f back
```

Our unit of work could be something like `a -> m b`. Our system could "just"
compose such units into larger "streams" and transfer inputs/outputs across
these units of work.

+ it looks a lot more "Haskell"
- it could be created on top of an actor system as a library



```haskell
[1] <> [2] <> [3] == [1, 2, 3]
```
