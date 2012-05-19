#lang racket

; Because of the way places work in Racket, I have a separate script to launch the program
; when using Racket's optimizing compiler. 

; The following shows how you would load and launch the program. You could do this from
; Racket's REPL, for example.

(require "functional_genetic_algorithm.rkt")

(run 100 100 8) ; 100 generations per cycle, 100 generations, 8 populations