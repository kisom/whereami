#!/bin/sh

STACK="stack --allow-different-user"
$STACK build && $STACK "$@"
