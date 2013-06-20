#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; population.rkt
; Mike Vollmer, 2012
;
; Tested in Racket 5.3.4 x84_64.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file is intended to be used as a module. Include it using (require "population.rkt").
;
; There are two ways to invoke this program. One is with run-population and one is with run-parallel-population.
; For a more complete explanation see the README, but essentially the difference is run-population only runs a
; single population while run-parallel-population runs several. 
;
; The arguments to both of the main functions use keywords, so they can be specified in any order. Some of the
; parameters are optional. Because pool and best-individual are optional keywords, it's possible to resume
; a search with a given pool and best-found individual.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/flonum)

(provide run-population flip (struct-out individual) (struct-out population))

(struct individual (value string fitness)) ; structure for each individual
(struct population (pool best)) ; whole population

; some helper functions for operations needing randomness
(define (flip prob) (if (< (random) prob) 1 0))
(define (random-flip-list n)
  (cond ((= n 0) '())
        (else (cons (flip 0.5) (random-flip-list (- n 1))))))
(define (random-flip-vector l) (list->vector (random-flip-list l)))
(define (random-index limit)
  (inexact->exact (round (* (- limit 1) (random)))))

(define (run-population
         #:pool [pool '()]
         #:best-individual [best '()]
         #:iterations iterations
         #:population-size population-size
         #:chromosome-length chromosome-length
         #:mutation-rate [mutation-rate 0.05]
         #:gene-size gene
         #:elite [elite-on #t]
         #:inversion-rate [inversion-rate 0.01]
         #:evaluation evaluate
         #:report [report-function '()]
         #:maxmin [maxmin 1]
         #:range-size [range-size 1024.0]
         #:range-offset [range-offset 512.0]
         #:criteria [criteria '()])
  
  ; decode takes a bit string and decodes it into numbers (returns a list of flonums)
  (define (decode str)
    ; str is a vector
    (letrec ((sum-str (lambda (str i) (if (= i -1) 0
                                          (+ (* (vector-ref str i) (expt 2 (- gene 1 i)))
                                             (sum-str str (- i 1))))))
             (split-vectors (lambda (i) (if (= i 0) '()
                                            (cons (vector-copy str (- i gene) i)
                                                  (split-vectors (- i gene)))))))
      (map (lambda (str) (->fl (sum-str str (- (vector-length str) 1)))) (split-vectors (vector-length str)))))
  
  (define (convrange raw)
    (map (lambda (i)
           (fl- (fl/ (fl* range-size (exact->inexact i)) 
                     (expt 2.0 gene)) 
                range-offset)) 
         raw))
  
  
  (define (elite pool best)
    ; take the best individual so far and add it to the pool
    (if elite-on
        (cons best (cdr (shuffle pool)))
        pool))
  
  ; these helper functions are for the tournament selection
  ; the code was getting long and complicated so I pulled out sections
  ; that were often repeated. the "a b c d" in the parameters is to make
  ; the tournament-selection function easier to read, but in some cases
  ; it turns out that some of the parameters could be assumed or derived
  ; rather than explicitly passed. I may go back and do that, but I feel
  ; like it would make the function even weirder to read.
  (define (compare-fitness vector-pool op a b)
    ; This function assumes pool has been converted to a vector
    ; for more efficient random access.
    (op (* maxmin
           (individual-fitness (vector-ref vector-pool a)))
        (* maxmin
           (individual-fitness (vector-ref vector-pool b)))))
  (define (better-fit-individual a b)
    ; compare two individuals directly, return better one
    (if (> (* maxmin (individual-fitness a))
           (* maxmin (individual-fitness b))) a b))
  (define (tournament-selection-compare pool-vector a b c d)
    ; I made this a function because the same lines of code showed up a
    ; few times below. basically, a and c here are going to be the same
    ; when the function is called (see above comment), so what it's doing
    ; is comparing a to b and a to d.
    (or
     (compare-fitness pool-vector >= a b)
     (compare-fitness pool-vector >= c d)))
  (define (build-tournament-result comparison a b c d)
    ; build the sublist that will be appended to the "sel" list
    (if comparison
        (list a b)
        (list c d)))
  
  (define (tournament-selection i sel pool-vector)
    ; Takes a pool (in vector form) and returns a list of selected indecies.
    (cond ((= i (vector-length pool-vector)) sel)
          (else
           ; generate three random index values (random whole numbers < pool length)
           (let* ((r (random-index (vector-length pool-vector)))
                  (s (random-index (vector-length pool-vector)))
                  (t (random-index (vector-length pool-vector)))
                  (new-sel
                   (cond ((tournament-selection-compare pool-vector r s r t)
                          (append (build-tournament-result 
                                   (compare-fitness pool-vector > s t) r s r t) sel))
                         ((tournament-selection-compare pool-vector s r s t)
                          (append (build-tournament-result 
                                   (compare-fitness pool-vector > r t) s r s t) sel))
                         ((tournament-selection-compare pool-vector t r t s)
                          (append (build-tournament-result 
                                   (compare-fitness pool-vector > r s) t r t s) sel)))))
             (tournament-selection (+ i 2) new-sel pool-vector)))))
  
  (define (tournament pool)
    ; calls the above function after making pool a vector
    (tournament-selection 0 '() (list->vector pool)))
  
  (define (initialize-population-pool s l)
    ; s is size of population, l is string length
    ; returns a list of individuals generated randomly
    (if (= s 0) '()
        (let* ((str (random-flip-vector l))
               (decoded (convrange (decode str))))
          ; I like to make sure the individual structure is always consistent
          ; even though the way the program works right now it isn't necessary.
          ; I could potentially save the computer some work and not calculate the
          ; fitness or the value until it's necessary, but I like to make sure 
          ; they're up-to-date by setting them every time I set the string.
          ; this is a place where I might go back and optimize if I need to
          (cons (individual decoded str (evaluate decoded))
                (initialize-population-pool (- s 1) l)))))
  
  (define (get-best-individual pool)
    ; pool is a list
    ; use argmax to find best fitness
    (argmax (lambda (indv) (* maxmin (individual-fitness indv))) pool))
  
  (define (combine-at v1 v2 i)
    ; dealing with vectors in a functional way is less intuitive than
    ; dealing with lists, imo, but it works out fine. since I'm not
    ; allowing myself to use set! or vector-set! or any such impure function,
    ; I combine two functions at a point using vector-take and vector-drop.
    (vector-append (vector-take v1 i) (vector-drop v2 i)))
  
  (define (crossover-compute vector-pool new-pool selected)
    ; pool is a list, new-pool is an empty list
    (if (null? selected) new-pool
        (let* ((site (inexact->exact 
                      (round (* (random) 
                                (- (vector-length (individual-string (vector-ref vector-pool 0)))
                                   1)))))
               ; let* executes these in order
               ; I like to use let* when I have a bunch of things to do in order like this
               ; where the previous result is used in the current computation.
               ; it would have been uglier imo if it had been written out as function calls
               ; and parameters.
               (parent1 (vector-ref vector-pool (car selected)))
               (parent2 (vector-ref vector-pool (car (cdr selected))))
               (rest-of-selected (cdr (cdr selected)))
               (parent1-string (individual-string parent1))
               (parent2-string (individual-string parent2))
               (child1-string (combine-at parent1-string parent2-string site))
               (child2-string (combine-at parent2-string parent1-string site))
               (decoded-child1 (convrange (decode child1-string)))
               (decoded-child2 (convrange (decode child2-string)))
               ; value and fitness will be computed by mutate, so they probably don't need to be computed here
               (child1 (individual decoded-child1
                                   child1-string (evaluate decoded-child1)))
               (child2 (individual decoded-child2 
                                   child2-string (evaluate decoded-child2)))
               (updated-new-pool (append new-pool (list child1 child2))))
          (crossover-compute vector-pool updated-new-pool rest-of-selected))))
  
  (define (crossover pool selected)
    ; calls crossover with a vector pool
    (crossover-compute (list->vector pool) '() selected))
  
  (define (invert-gene string)
    ; inversion is a trick to avoid hamming cliffs
    ; it's hard for a genetic algorithm to get an individual from 01111 to 10000 even though they're right
    ; next to each other numerically. periodically inverting all the bits in a gene can help it along.
    ; another potential solution would be to use gray codes.
    (if (or (= (flip inversion-rate) 0)
            (= (vector-length string) 0))
        string ; return unchanged
        (let ((split-start (if (= chromosome-length gene) 0
                               (random-index (- chromosome-length gene)))))
          (vector-append (vector-take string split-start) 
                         (vector-map (lambda (i) (- 1 i))
                                     (vector-copy string split-start (+ split-start gene)))
                         (vector-drop string (+ split-start gene))))))
  
  (define (mutation pool)
    ; randomly mutate parts of a bit string
    (let ((mutate-individual (lambda (indv)
                               (let* ((str (vector-map (lambda (i) 
                                                        (if (= (flip mutation-rate) 1) (- 1 i) i))
                                                      (individual-string indv)))
                                      (decoded (convrange (decode str))))
                                 (individual decoded
                                             (invert-gene str) 
                                             (evaluate decoded))))))
      (map mutate-individual pool)))
  
  (define (step pool best i)
    ; execute one generation
    (if (and (not (null? report-function)) (not (null? best)))
        (report-function i (individual-value best)) '())
    (if (null? pool)
        (let ((initial-population (initialize-population-pool population-size chromosome-length)))
          (step initial-population (get-best-individual initial-population) i))
        (if (= i 0) (population pool best)
            ; I'm using let* again to make the flow of data easier to read
            (let* ((temp-pool (mutation (crossover pool (tournament pool))))
                   (gen-best (get-best-individual pool))
                   (best-individual (better-fit-individual best gen-best))
                   (new-pool (elite temp-pool best-individual)))
              (if (and (number? criteria) (< (abs (individual-fitness best-individual)) criteria))
                  (population pool best)
                  (step new-pool best-individual (- i 1)))))))
  
  (step pool best iterations))