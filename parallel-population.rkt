#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; parallel-population.rkt
; Mike Vollmer, 2012
;
; Tested in Racket 5.3.4 x84_64.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file is intended to be used as a module. Include it using (require "parallel-population.rkt").
;
; This is an attempt at incorporating Racket Places to do real parallelism in this parallel genetic algorithm
; implementation.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; the next few functions allow the individuals to migrate between populations
; this is done by recursing down the population list, taking one individual from each,
; shuffling that list of individuals, and recursing down the population list again
; merging the individuals back in (in random order)

(require "population.rkt")

(provide run-parallel-population)

(define (create-cross-population-list pop-list)
  (shuffle (map (lambda (pop) (car (population-pool pop))) pop-list)))

(define (cross-populations pop-list cross-list)
  (if (null? pop-list) '()
      (let ((cross (lambda (pop indv) 
                     (population 
                      (shuffle (cons indv (cdr (population-pool pop))))
                      (population-best pop)))))
        (cons (cross (car pop-list) (car cross-list))
              (cross-populations (cdr pop-list) (cdr cross-list))))))

(define (build-pop-list n)
  (build-list n (lambda (i) (population '() '()))))

(define (do-cross pop-list)
  (cross-populations pop-list (create-cross-population-list pop-list)))

(define (run-parallel-population
         #:parallel-populations parallel-count
         #:parallel-cycles cycles
         #:pool [pool '()]
         #:best-individual [best '()]
         #:iterations iterations
         #:population-size population-size
         #:chromosome-length chromosome-length
         #:mutation-rate [mutation-rate 0.05]
         #:gene-size gene
         #:elite [elite-on #t]
         #:inversion-rate [inversion-rate 0.01]
         #:evaluation evaluate-code
         #:report [report-function '()]
         #:maxmin [maxmin 1]
         #:range-size [range-size 1024.0]
         #:range-offset [range-offset 512.0]
         #:criteria [criteria '()])
  (letrec ((evaluate (eval evaluate-code (make-base-namespace)))
           (parallel-cycle 
            (lambda (i pop-list) (if (= i 0) pop-list
                                     (parallel-cycle (- i 1)
                                                     (do-cross (map (lambda (pop)
                                                                      (run-population
                                                                       #:pool (population-pool pop)
                                                                       #:best-individual (population-best pop)
                                                                       #:iterations iterations
                                                                       #:population-size population-size
                                                                       #:chromosome-length chromosome-length
                                                                       #:mutation-rate mutation-rate
                                                                       #:gene-size gene
                                                                       #:elite elite-on
                                                                       #:inversion-rate inversion-rate
                                                                       #:evaluation evaluate
                                                                       #:report report-function
                                                                       #:maxmin maxmin
                                                                       #:range-size range-size
                                                                       #:range-offset range-offset
                                                                       #:criteria criteria))
                                                                    pop-list)))))))
    (parallel-cycle cycles (build-pop-list parallel-count))))