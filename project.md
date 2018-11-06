# whereami?
## Project planning

Oneliner: self-hosted web service for storing geographic location history.

## Milestones
1. JSON POST, text GET with coordinates and basic auth
2. TOTP update
3. SQLite integration
4. Web page that asks for location (e.g. using browser mechanism) and uses that to update

## Packages
+ servant: web framework
+ aeson
+ totp?
+ sqlite?

## Punting on
+ 2018-11-05 - altitude (too many type errors, cutting search tree for now)
