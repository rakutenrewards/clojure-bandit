# curbside-bandit

This is a library of algorithms for efficiently solving multi-armed bandit (MAB) problems. You can think of MAB problems as A/B tests, except requiring less human intervention -- the bandit algorithm automatically assigns larger amounts of traffic to better alternatives in a statistically sound way. 

This library is designed for use in systems that need to make thousands of decisions per second with low latency. It supports multiple backends for storing algorithm state, allowing the use of e.g. Redis to enable long-term experiments in distributed systems.

For now, this is just a library. In the future, we will add a web service wrapper as well.

## Usage

See the documentation in the `curbside.bandit.core` namespace.

## Development

This project uses `lein` in the standard way. `lein repl`, `lein test`, etc. `lein codox` generates documentation, which might be easier to read than the raw source files.
