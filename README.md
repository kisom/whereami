# whereami

I wanted a Haskell project to hack on, and this seems like a pretty good
option. It's something I've wanted for a while, and I could build for
myself to get exactly what I want to fit my security model and whatnot.

## Organization

- The main app setup occurs in `app/Main.hs`.
- `src/Location/Core.hs` contains the core coordinates type and utilities.
- `src/Location/DB.hs` contains the SQLite layer over the `Coordinates` type.
- `src/Location/API.hs` contains various API functions.

There's an unimplemented test suite that Stack setup for me that I haven't touched.

## The stack

The stack currently used is

+ Haskell (Scotty/Warp/WAI, sqlite-simple, and aeson) for the backend
+ POJS and HTML for the frontend (there's no CSS at the moment)
+ SQLite3 for the database
+ Stackage for the build system, although there's a Makefile for convenience
+ Docker and docker-compose for deployment

## Dependencies

On Ubuntu, install `haskell-stack`.

## Running

- `stack setup` - only needed the first time you run this.
- `stack run`

This will set up a dev instance listening on port 4000 with the default user
and password of 'kyle:password'.

## Deploying

The `docker-compose.yaml.in` contains a basic framework for running this.