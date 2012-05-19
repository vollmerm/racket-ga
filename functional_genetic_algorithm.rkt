#lang racket

(require racket/flonum)
(require racket/place)

(provide run)

(define +population-size+ 20)
(define +chrom-length+ 100)
(define +pmut+ 0.05)
(define +max-gen+ 10000)
(define +gen-rep+ 500)
(define +elite+ #t)
(define +maxmin+ -1)
(define +gene+ 10)
(define +invert-gene+ #t) ; occasionally invert the bits in a gene
(define +inversion-rate+ 0.005)

(struct individual (value string fitness))
(struct population (pool best))

(define (flip prob) (if (< (random) prob) 1 0))
(define (random-flip-list n)
  (cond ((= n 0) '())
        (else (cons (flip 0.5) (random-flip-list (- n 1))))))
(define (random-flip-vector l) (list->vector (random-flip-list l)))
(define (random-index limit)
  (inexact->exact (round (* (- limit 1) (random)))))

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
  (or
   (compare-fitness pool-vector >= a b)
   (compare-fitness pool-vector >= c d)))
(define (build-tournament-result comparison a b c d)
  (if comparison
      (list a b)
      (list c d)))

(define (tournament-selection i sel pool-vector)
  ; Takes a pool (in vector form) and returns a list of selected indecies.
  (cond ((= i (vector-length pool-vector)) sel)
        (else
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
  (tournament-selection 0 '() (list->vector pool)))

(define (initialize-population-pool s l)
  ; s is size of population, l is string length
  ; returns a list of individuals generated randomly
  (if (= s 0) '()
      (let ((str (random-flip-vector l)))
        (cons (individual (decode str) str (evaluate (decode str)))
              (initialize-population-pool (- s 1) l)))))

(define (get-best-individual pool)
  ; pool is a list
  ; use argmax to find best fitness
  (argmax (lambda (indv) (* +maxmin+ (individual-fitness indv))) pool))

(define (combine-at v1 v2 i)
  (vector-append (vector-take v1 i) (vector-drop v2 i)))

(define (crossover-compute vector-pool new-pool selected)
  ; pool is a list, new-pool is an empty list
  (if (null? selected) new-pool
      (let* ((site (inexact->exact 
                    (round (* (random) 
                              (- (vector-length (individual-string (vector-ref vector-pool 0)))
                                 1)))))
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

;(define (invert-gene pool)
;  (if (= (flip +inversion-rate+) 0)
;      pool
;      (letrec ((split-start (random-index (- (length pool) +gene+)))
;               (invert-tail (lambda (i pool new-pool)
;                              (cond ((null? pool) new-pool)
;                                    ((< i split-start) 
;                                     (invert-tail (+ i 1) (cdr pool) (cons (car pool) new-pool)))
;                                    ((and (>= i split-start) (< i (+ split-start +gene+)))
;                                     (invert-tail (+ i 1) (cdr pool) (cons (- 1 (car pool)) new-pool)))
;                                    ((>= i (+ split-start +gene+))
;                                     (invert-tail (+ i 1) (cdr pool) (cons (car pool) new-pool)))))))
;        (invert-tail 0 pool '()))))

(define (invert-gene string)
  (if (= (flip +inversion-rate+) 0)
      string ; return unchanged
      (let ((split-start (random-index (- (vector-length string) +gene+))))
        (vector-append (vector-take string split-start) 
                       (vector-map (lambda (i) (- 1 i))
                                   (vector-copy string split-start (+ split-start +gene+)))
                       (vector-drop string (+ split-start +gene+))))))

;(define (mutation pool chance)
;  ; pool is a list, chance is a float
;  (map (lambda (indv)
;         (struct-copy individual indv
;                      [string (vector-map (lambda (i) (if (= (flip chance) 1) (- 1 i) i))
;                                          (individual-string indv))]))))

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
  (map (lambda (i)
         (fl- (exact->inexact i) 512.0)) raw))

(define (evaluate value-list)
  (let ((value-list (convrange value-list)))
    (letrec ((find-in-list (lambda (i lst item)
                             (cond ((null? lst) -1)
                                   ((eq? (car lst) item) i)
                                   (#t (find-in-list (+ i 1) (cdr lst) item))))))
      (fl- (foldl (lambda (i s) (fl+ s (fl/ (fl* i i) 4000.0))) 1.0 value-list)
           (foldl (lambda (i s) (fl* s (cos (fl/ i (sqrt (fl+ 1.0 (find-in-list 0.0 value-list i))))))) 
                  1.0 value-list)))))

(define (elite pool best)
  (if +elite+
      (cons best (cdr (shuffle pool)))
      pool))

(define (step pop i)
  (let ((pool (population-pool pop))
        (best (population-best pop)))
    (if (= i 0) pop
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

(define (population->vector pop)
  (vector (map individual->vector (population-pool pop)) (individual->vector (population-best pop))))

(define (individual->vector indv)
  (vector (individual-value indv) (individual-string indv) (individual-fitness indv)))

(define (vector->individual v)
  (individual (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)))

(define (vector->population v)
  (population (map vector->individual (vector-ref v 0)) (vector->individual (vector-ref v 1))))

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
  (display "Simple Parallel Genetic Algorithm in Racket")(newline)
  (display "Mike Vollmer, 2012")(newline)(newline)
  (let* ((result (cycle n i (build-pop-list p)))
         (best (find-best result)))
    (print-pop-list result)
    (newline)(display "* Most fit *")(newline)
    (display "Fitness: ")(display (individual-fitness (population-best best)))(newline)
    (display "Value: ")(display (convrange (individual-value (population-best best))))(newline)))
