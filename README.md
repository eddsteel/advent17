# advent17

[![Build Status](https://travis-ci.org/eddsteel/advent17.svg?branch=master)](https://travis-ci.org/eddsteel/advent17)


## Run

Create a `secrets` file with your advent of code cookie in it:

```
fork in Run := true
envVars in Run := Map("COOKIE" -> "_ga=<ga>; _gid=<gid>; session=<session>; _gat=<gat>")

```

`. secrets` then `sbt run`.
