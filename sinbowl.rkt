#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; sinbowl.rkt
; Mike Vollmer, 2013
;
; This is an example. It shows how you can call racket-ga from another Racket module, including how to define
; an evaulation function and a reporting function.
;
; The following code attempts to minimize the "sinbowl" function, which is f(x) = (0.1 * abs(x)) - sin(x)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "population.rkt")
(require "parallel-population.rkt")

(define evaluate
  '(lambda (lst) (let ((sinbowl (lambda (value) (- (* (abs value) 0.1) (sin value)))))
    (sinbowl (car lst)))))

(define report
  '(lambda (i value)
           (if (= (modulo i 50) 0)
               (begin (display (car value))(newline)) '())))

(define (run-parallel)
  (display "racket-ga example")(newline)
  (display "Minimixing sinbowl function")(newline)(newline)
  
  (define pop-list (run-parallel-population
                    #:parallel-populations 3
                    #:parallel-cycles 10
                    #:iterations 100
                    #:population-size 50
                    #:chromosome-length 30
                    #:gene-size 30
                    #:evaluation evaluate
                    #:report report
                    #:maxmin -1
                    #:range-size 120.0
                    #:range-offset 60.0))

  (define best-individual 
    (population-best (argmin (lambda (pop) (individual-fitness (population-best pop))) pop-list)))

  ; print out the results
  (display "Results of minimizing sinbowl:")(newline)
  (display "Fitness: ")(display (individual-fitness best-individual))(newline)
  (display "Value: ")(display (individual-value best-individual))(newline)
  (display "DNA: ")(display (individual-string best-individual))(newline))

(define (run-normal)
  (display "racket-ga example")(newline)
  (display "Minimixing sinbowl function")(newline)(newline)
  
  (define pop (run-population
               #:iterations 100
               #:population-size 50
               #:chromosome-length 30
               #:gene-size 30
               #:evaluation evaluate
               #:report report
               #:maxmin -1
               #:range-size 120.0
               #:range-offset 60.0))
  
  (define best-individual 
    (population-best pop))

  ; print out the results
  (display "Results of minimizing sinbowl:")(newline)
  (display "Fitness: ")(display (individual-fitness best-individual))(newline)
  (display "Value: ")(display (individual-value best-individual))(newline)
  (display "DNA: ")(display (individual-string best-individual))(newline))
                    