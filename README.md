# Computability Zoo

Eventually this is meant to be an experiment in translating among different
equivalent models of computation. Ultimately I'd like to support bidirectional
conversions among the following:

+ The lambda calculus
+ Turing machines
+ General recursive functions

Currently, however, this project lives as a basic arithmetic calculator
on the natural numbers implemented with the lambda calculus (that is arithmetic
operations such as `+` or `*` are implemented through the lambda calculus
without delegating directly to the built-in addition or multiplication operators
of the implementation language).

Note that because we operate on the natural numbers, subtraction and division
are not defined (maybe I'll get around to defining)

## Running this application

For ease of use this application ships as a dirt-simple webpage. Look at the
[releases](https://github.com/changlinli/computability-zoo/releases/tag/v0.1.0
"Latest release") for an `index.html` and a `computability-zoo-opt.js` file. As
long as you put both files in the same directory and open `index.html` with a
web browser, things should work.

## Building this application

This application is written in Scala and built with SBT. You'll therefore have
to have SBT installed. It compiles to Javascript through ScalaJS.

If you do have SBT installed, it's a simple matter of running the following
(assuming you are in the root directory of this project).

```
sbt computabilityZooJS/fullOptJS

# Copy the generated Javascript file over
cp js/target/scala-2.12/computability-zoo-opt.js .

# Open up the HTML page
$YOURBROWSEROFCHOICE index.html
```
