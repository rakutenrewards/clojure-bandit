# curbside-bandit

This is a library of algorithms for efficiently solving multi-armed bandit (MAB) problems. You can think of MAB problems as A/B tests, except requiring less human intervention -- the bandit algorithm automatically assigns larger amounts of traffic to better alternatives in a statistically sound way.

This library is designed for use in systems that need to make thousands of decisions per second with low latency. It supports multiple backends for storing algorithm state, allowing the use of e.g. Redis to enable long-term experiments in distributed systems.

For now, this is just a library. In the future, we will add a web service wrapper as well.

## Usage

See the documentation in the `curbside.bandit.core` namespace.

### Available state storage providers

The library provides support for using either `(atom {})` or a Carmine Redis connection to store state. Obviously, you must use Redis if you want to persist learner state, put a service behind a load balancer, etc. See the tests for examples of using both backends.

The Redis backend can handle roughly ten thousand operations (`choose` and `reward`) per second. To scale further, we would need to switch to Redis Cluster, or manually distribute the learners across separate Redis servers. The atom backend can handle about ten times that.

### Available algorithms

#### Epsilon-greedy

The simplest algorithm available. You provide a parameter `0 < epsilon < 1.0` and epsilon-greedy chooses the best arm with probability `1.0 - epsilon`. Otherwise, it chooses a random arm. The downside is that it will continue trying random arms forever, even after it has become obvious which arm is best.

#### UCB1

For most problems, this should be the default choice. It performs very well, and doesn't have any parameters to tune. However, a possible downside is that if rewards are significantly delayed, it will deterministically assign all traffic to one arm until it receives rewards. This could have unintended consequences.

#### Softmax

For complex use cases only. This requires a temperature parameter to be specified. When the learner starts, its temperature is high, and it will choose arms randomly. As the temperature cools, it will begin to choose arms that have performed best so far. This is similar in principle to simulated annealing. This has the advantage over UCB1 that it will try all the arms in the beginning, even when rewards are delayed. The disadvantage is that if you add a new arm after it has cooled, it might never try it.

## Development

This project uses `lein` in the standard way. `lein repl`, `lein test`, etc. `lein codox` generates documentation, which might be easier to read than the raw source files.
