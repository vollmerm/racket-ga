#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; functional_genetic_algorithm.rkt
; Mike Vollmer, 2012
;
; Based on sga.cpp from Dr. Gordon of CSU Sacramento, 
;   which in turn was adapted from David Goldberg:
;     "Genetic Algorithms in Search, Optimization, and Machine Learning"
;     Addison-Wesley, 1989.
;
; Tested in Racket 5.2.1 x84_64. It should work both in DrRacket and with Racket's optimizing compiler.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This code is currently set up to minimize the Griewank function, which is defined as:
;
; minimize f(x[i]) = 
;   1 +   sum over all x[i]: (x[i]^2 / 4000)
;   minus product over all x[i]: (cos (x[i]/sqrt(i))
;
; To run it, use the run function, (run n i p), where:
;   n is the number of generations per cycle (between population crossovers)
;   i is the number of cycles
;   p is the number of populations (and number of threads to spawn)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require racket/flonum)
(require racket/place)

(provide run) ; export the run function, which runs the rest of the program

(define +population-size+ 20) ; number of individuals in the population
(define +chrom-length+ 100) ; number of chromosomes (bits in bitstring)
(define +pmut+ 0.05) ; probability of mutation
(define +elite+ #t) ; turn elitism on or off
(define +maxmin+ -1) ; -1 for minimizing, 1 for maximizing fitness
(define +gene+ 10) ; length of gene
(define +invert-gene+ #t) ; occasionally invert the bits in a gene
(define +inversion-rate+ 0.005) ; probability of gene inversion

(struct individual (value string fitness)) ; structure for each individual
(struct population (pool best)) ; keep track of best of population in this structure

; some helper functions for operations needing randomness
(define (flip prob) (if (< (random) prob) 1 0))
(define (random-flip-list n)
  (cond ((= n 0) '())
        (else (cons (flip 0.5) (random-flip-list (- n 1))))))
(define (random-flip-vector l) (list->vector (random-flip-list l)))
(define (random-index limit)
  (inexact->exact (round (* (- limit 1) (random)))))

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
  (op (* +maxmin+
         (individual-fitness (vector-ref vector-pool a)))
      (* +maxmin+
         (individual-fitness (vector-ref vector-pool b)))))
(define (better-fit-individual a b)
  ; compare two individuals directly, return better one
  (if (> (* +maxmin+ (individual-fitness a))
         (* +maxmin+ (individual-fitness b))) a b))
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
      (let ((str (random-flip-vector l)))
        ; I like to make sure the individual structure is always consistent
        ; even though the way the program works right now it isn't necessary.
        ; I could potentially save the computer some work and not calculate the
        ; fitness or the value until it's necessary, but I like to make sure 
        ; they're up-to-date by setting them every time I set the string.
        ; this is a place where I might go back and optimize if I need to
        (cons (individual (decode str) str (evaluate (decode str)))
              (initialize-population-pool (- s 1) l)))))

(define (get-best-individual pool)
  ; pool is a list
  ; use argmax to find best fitness
  (argmax (lambda (indv) (* +maxmin+ (individual-fitness indv))) pool))

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
             ; value and fitness will be computed by mutate, so they probably don't need to be computed here
             (child1 (individual (decode child1-string) child1-string (evaluate (decode child1-string))))
             (child2 (individual (decode child2-string) child2-string (evaluate (decode child2-string))))
             (updated-new-pool (append new-pool (list child1 child2))))
        (crossover-compute vector-pool updated-new-pool rest-of-selected))))

(define (crossover pool selected)
  (crossover-compute (list->vector pool) '() selected))

(define (invert-gene string)
  ; inversion is a trick to avoid hamming cliffs
  ; it's hard for a genetic algorithm to get an individual from 01111 to 10000 even though they're right
  ; next to each other numerically. periodically inverting all the bits in a gene can help it along.
  ; another potential solution would be to use gray codes.
  (if (= (flip +inversion-rate+) 0)
      string ; return unchanged
      (let ((split-start (random-index (- (vector-length string) +gene+))))
        (vector-append (vector-take string split-start) 
                       (vector-map (lambda (i) (- 1 i))
                                   (vector-copy string split-start (+ split-start +gene+)))
                       (vector-drop string (+ split-start +gene+))))))

(define (mutation pool chance)
  (let ((mutate-individual (lambda (indv)
                             (let ((str (vector-map (lambda (i) (if (= (flip chance) 1) (- 1 i) i))
                                                    (individual-string indv))))
                               (individual (decode str) (invert-gene str) (evaluate (decode str)))))))
    (map mutate-individual pool)))

(define (decode str)
  ; str is a vector
  (letrec ((sum-str (lambda (str i) (if (= i -1) 0
                                        (+ (* (vector-ref str i) (expt 2.0 (- +gene+ 1 i)))
                                           (sum-str str (- i 1))))))
           (split-vectors (lambda (i) (if (= i 0) '()
                                          (cons (vector-copy str (- i +gene+) i)
                                                (split-vectors (- i +gene+)))))))
    (map (lambda (str) (sum-str str (- (vector-length str) 1))) (split-vectors (vector-length str)))))

(define (convrange raw)
  ; in the case of this problem (range -512 to 512), all that's needed to convert the range
  ; is subtracting 512. in most problems this function would also need multiplication and division
  (map (lambda (i)
         (fl- (exact->inexact i) 512.0)) raw))

(define (evaluate value-list)
  ; this is the function that determines the fitness
  ; I'm not happy with it right now. it needs to see the index of the number in its list, which is not
  ; provided by foldl, so I'm looking it up separately (in an inefficient way). there's also a potential
  ; problem with a number being in the list twice. this function needs to be re-written.
  (let ((value-list (convrange value-list)))
    (letrec ((find-in-list (lambda (i lst item)
                             (cond ((null? lst) -1)
                                   ((eq? (car lst) item) i)
                                   (#t (find-in-list (+ i 1) (cdr lst) item))))))
      (fl- (foldl (lambda (i s) (fl+ s (fl/ (fl* i i) 4000.0))) 1.0 value-list)
           (foldl (lambda (i s) (fl* s (cos (fl/ i (sqrt (fl+ 1.0 (find-in-list 0.0 value-list i))))))) 
                  1.0 value-list)))))

(define (elite pool best)
  ; take the best individual so far and add it to the pool
  (if +elite+
      (cons best (cdr (shuffle pool)))
      pool))

(define (step pop i)
  ; execute one generation
  (let ((pool (population-pool pop))
        (best (population-best pop)))
    (if (= i 0) pop
        ; I'm using let* again to make the flow of data easier to read
        (let* ((temp-pool (mutation (crossover pool (tournament pool)) +pmut+))
               (gen-best (get-best-individual pool))
               (best-individual (better-fit-individual best gen-best))
               (new-pool (elite temp-pool best-individual)))
          (step (population new-pool best-individual) (- i 1))))))

(define initial-best-fitness
  (if (= +maxmin+ -1) 999999 -999999))

(define initial-best-individual
  (individual 0 (make-vector +population-size+) initial-best-fitness))

(define initial-pool (initialize-population-pool +population-size+ +chrom-length+))

; the next few functions allow the individuals to migrate between populations
; this is done by recursing down the population list, taking one individual from each,
; shuffling that list of individuals, and recursing down the population list again
; merging the individuals back in (in random order)

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

(define (do-cross pop-list)
  (cross-populations pop-list (create-cross-population-list pop-list)))

(define (setup-population)
  (population (initialize-population-pool +population-size+ +chrom-length+)
              initial-best-individual))

(define (build-pop-list n)
  (let ((range (build-list n values)))
    (map (lambda (i) (setup-population)) range)))

; Racket doesn't let ordinary structs go in messages to places. I'm sure it has a good reason
; for that limitation. in my case, the structs are purely immutable (like everything in the program)
; but I still have to convert them to something I can give to a place. so these functions convert
; everything to vectors, and then conver everything back to structs.

(define (population->vector pop)
  (vector (map individual->vector (population-pool pop)) (individual->vector (population-best pop))))

(define (individual->vector indv)
  (vector (individual-value indv) (individual-string indv) (individual-fitness indv)))

(define (vector->individual v)
  (individual (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)))

(define (vector->population v)
  (population (map vector->individual (vector-ref v 0)) (vector->individual (vector-ref v 1))))

; this code constructs and uses the places
; the syntax is still new to me so I'm mimicing the code structure found in Racket's documentation

(define (make-process-place pop n)
  (define p
    (place ch
           (define initial (place-channel-get ch))
           (define new-pop (step (vector->population (car initial)) (cdr initial)))
           (place-channel-put ch (population->vector new-pop))))
  (place-channel-put p (cons (population->vector pop) n)) p)

(define (run-process-place pop-list n)
  (let ((place-list (map (lambda (pop) (make-process-place pop n)) pop-list)))
    (define result (map (lambda (p) (vector->population (place-channel-get p))) place-list))
    (map place-kill place-list)
    result))

(define (cycle n i pop-list)
  ; n is number of cycles between crossover
  ; i is number of crossovers
  (if (= i 0) pop-list
      (let ((cross (do-cross (run-process-place pop-list n))))
        (display "Cycles left: ")(display i)(newline)
        (print-population-best (find-best pop-list))
        (cycle n (- i 1) cross))))

(define (find-best pop-list)
  (argmax (lambda (pop) (* +maxmin+ (individual-fitness (population-best pop)))) pop-list))

(define (print-pop-list pop-list)
  (map (lambda (pop) (display (individual-fitness (population-best pop)))(newline)) pop-list))

(define (print-population-best pop)
  (let ((best (population-best pop)))
    (display "Best fitness: ")(display (individual-fitness best))(newline)
    (display (individual-string best))(newline)))

(define (run n i p)
  ; this is the end! just one function to put it all together.
  ; n is the number of generations per cycle (between population crossovers)
  ; i is the number of cycles
  ; p is the number of populations (and number of threads to spawn)
  (display "Simple Parallel Genetic Algorithm in Racket")(newline)
  (display "Mike Vollmer, 2012")(newline)(newline)
  (let* ((result (cycle n i (build-pop-list p)))
         (best (find-best result)))
    (print-pop-list result)
    (newline)(display "* Most fit *")(newline)
    (display "Fitness: ")(display (individual-fitness (population-best best)))(newline)
    (display "Value: ")(display (convrange (individual-value (population-best best))))(newline)))
