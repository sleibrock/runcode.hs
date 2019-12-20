runcode.hs
============

`runcode.hs` is a program aimed at running any bit of code given and dispatch it to the correct compiler. The idea stems from that of something like [Google Cloud Functions]() or [Amazon Lambda]() where you can run on a (serverless) architecture any code you want when requested. Although it may not reach that far, the goal is to start working on some similar functionality.

* Stage 1: build a program that will dispatch code to it's correct compiler/interpreter, build it, and execute the intermediate results (if any)
* Stage 2: do it over a network connection via HTTP calls
* Stage 3: deal with additional packages using a packaging service like Nix
* Stage 4: architecture (who knows if we'll get that far)
