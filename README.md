racket-ga
=========

This is a parallel genetic algorithm implementation in [Racket Scheme](http://racket-lang.org/), using [Places](http://docs.racket-lang.org/reference/places.html) for parallelism. It is written in a functional style and uses elitism, inversion, and subpopulations.

Usage
-----

### Invoking

See sinbowl.rkt for a full example of usage.

To use this module in your own code, it must be require'd.

```scheme
(require "population.rkt")
```

If you want to use the multi-process parallel features, do this instead:

```scheme
(require "population.rkt")
(require "parallel-population.rkt")
```

After that, there are two search procedures exposed. One is run-population, which runs a search with a single population. The other is run-parallel-population, which runs several populations separately and crosses over between them. 

Here's a simple example of invoking the search (taken from sinbowl.rkt):

```scheme
(run-population
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

The above code starts a search with 10 iterations, a population size of 50, a chromosome length of 30 bits, a gene size of 30, maxmin set to -1 (1 for maximize, -1 for minimize), and a range between -60 and +60.

_report_ and _evaluate_ are quoted lists representing functions. Because they will need to be passed between Racket processes, they cannot be closures, so I picked the next-best approach and just eval the code in each process to define the function. If this bothers you, or if you want to do something more significant that you can't do with the ordinary base environment, you can add your evaluation functions directly to population.rkt or parallel-population.rkt.

The report function takes two parameters: an index (between 0 and the iteration limit), and the best value found so far. The evaluate function takes a list of values as a parameter, each corresponding to a gene.

There are some other subtleties to the parameter list:

 * To only have a single value, set the gene size equal to the chromosome length. In this case, evaluate will be passed a list of length 1.
 * Both functions can optionally be passed pool and best-individual. These can be used to resume an old search.

There are other optional parameters:

 * elite: takes a boolean value, to enable or disable elitism
 * mutation-rate: takes a number between 0 and 1
 * inversion-rate: takes a number between 0 and 1

maxmin, range-size, and range-offset are also optional. They default to 1, 1024.0, and 512.0 respectively.

For an example of run-parallel-population, look at sinbowl.rkt. Note that the parallel search is still experimental and not properly tested.

### Structures

To use the module you'll likely need to interact with the structs defined in it. Luckily there are only two you'll need to use, and they're very simple.

```scheme
(struct individual (value string fitness) #:prefab) ; structure for each individual
(struct population (pool best) #:prefab) ; whole population
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

Misc
----

### Sources

A very early version of this code was ported from sga.cpp, a program provided by my professor, Dr. Gordon from CSU Sacramento, and based on code by David Goldberg. It contained a reference to "Genetic Algorithms in Search, Optimization, and Machine Learning," Addison-Wesley, 1989.

None of that original code remains (it was re-written a while before I started this repository), but I thought I should mention where it started. 
