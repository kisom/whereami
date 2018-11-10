# whereami

I wanted a Haskell project to hack on, and this seems like a pretty good
option. It's something I've wanted for a while, and I could build for
myself to get exactly what I want to fit my security model and whatnot.

## Dependencies

On Ubuntu, install `haskell-stack`.

## Running

- `stack setup` - only needed the first time you run this.
- `stack run`

This will set up a dev instance listening on port 4000 with the default user
and password of 'kyle:password'.

## Deploying

The `docker-compose.yaml.in` contains a basic framework for running this.