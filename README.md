racket-ga
=========

This is a parallel genetic algorithm implementation in Racket Scheme. It is written in a functional style and uses elitism, inversion, and subpopulations.

Usage
-----

### Invoking

See sinbowl.rkt for a full example of usage.

To use this module in your own code, it must be require'd.

```scheme
(require "population.rkt")
```

After that, there are two search procedures exposed. One is run-population, which runs a search with a single population. The other is run-parallel-population, which runs several populations separately and crosses over between them. 

Here's a simple example of invoking the search (taken from sinbowl.rkt):

```scheme
(run-parallel-population
 #:parallel-populations 5
 #:parallel-cycles 5
 #:iterations 10
 #:population-size 50
 #:chromosome-length 30
 #:gene-size 30
 #:evaluation evaluate
 #:report report
 #:maxmin -1
 #:range-size 120.0
 #:range-offset 60.0)
```

The above code starts a search with 5 parallel populations, 10 iterations per cycle (meaning generations that occur between the crossovers from one population to another), 5 cycles (meaning the number of times individuals cross over between populations), a population size of 50, a chromosome length of 30 bits, a gene size of 30, maxmin set to -1 (1 for maximize, -1 for minimize), and a range between -60 and +60.

There are some other subtleties to the parameter list:

 * To only have a single value, set the gene size equal to the chromosome length. In this case, evaluate will be passed a list of length 1.
 * Both functions can optionally be passed pool and best-individual. These can be used to resume an old search.

There are other optional parameters:

 * elite: takes a boolean value, to enable or disable elitism
 * mutation-rate: takes a number between 0 and 1
 * inversion-rate: takes a number between 0 and 1

maxmin, range-size, and range-offset are also optional. They default to 1, 1024.0, and 512.0 respectively.

### Structures

To use the module you'll likely need to interact with the structs defined in it. Luckily there are only two you'll need to use, and they're very simple.

```scheme
(struct individual (value string fitness)) ; structure for each individual
(struct population (pool best)) ; whole population
```

run-population returns a population structure, while run-parallel-population returns a list of them. The pool in the population structs is a list of individuals.

In case you aren't familiar with how to use structs in Racket scheme, here's a relevent section of sinbowl.rkt demonstrating access to an individual structure:

```scheme
(display "Results of minimizing sinbowl:")(newline)
(display "Fitness: ")(display (individual-fitness best-individual))(newline)
(display "Value: ")(display (individual-value best-individual))(newline)
(display "DNA: ")(display (individual-string best-individual))(newline)
```

The "fitness" field of struct "individual" is accessed with "individual-fitness" and so on. More on this subject can be found in Racket's documentation: http://docs.racket-lang.org/reference/structures.html

Limitations
-----------

run-parallel-population currently does not take advantage of hardware parallelism (meaning, the populations run in parallel metaphorically, but as far as your cpu is concerned they're all in the same thread). If you absolutely need true parallelism, you can use Racket's places, having each place call run-population (calls to run-population and run-parallel-population are restartable/resumable). This is fairly easy to do if you use population.rkt as a reference. 

I wasn't able to do that automatically because of the nature of places in Racket, and how I designed the API. Another program using racket-ga can submit an evaluation function, but evaluation functions cannot be sent to places, and places are only closed over top-level definitions that are set before the code is run. 

However, it's certainly possible to write your own code to create the places and have them call run-population repeatedly, given your evaluation and report functions are defined at the top level of your module. If you want, you can look up the initial committed code of this repository to see an example of how I did it.

Misc
----

### Sources

A very early version of this code was ported from sga.cpp, a program provided by my professor, Dr. Gordon from CSU Sacramento, and based on code by David Goldberg. It contained a reference to "Genetic Algorithms in Search, Optimization, and Machine Learning," Addison-Wesley, 1989.

None of that original code remains (it was re-written a while before I started this repository), but I thought I should mention where it started. 