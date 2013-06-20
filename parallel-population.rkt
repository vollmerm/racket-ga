#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; parallel-population.rkt
; Mike Vollmer, 2013
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

;;; TODO: Add explanatory comments to this code!

(require "population.rkt")

(provide run-parallel-population)

(struct island (pl pop) #:prefab)

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

(define (do-cross-with-places island-list)
  (map island (map island-pl island-list) (do-cross (map island-pop island-list))))

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
  (letrec ((param-set (list iterations population-size chromosome-length
                            mutation-rate gene elite-on inversion-rate 
                            evaluate-code report-function maxmin range-size 
                            range-offset criteria))
           (pop-places (map 
                        (lambda (x) 
                          (place ch
                                 (define (run-iteration)
                                   (define data (place-channel-get ch))
                                   (define pop (car data))
                                   (define local-params (cadr data))
                                   (place-channel-put 
                                    ch 
                                    (run-population
                                     #:pool (population-pool pop)
                                     #:best-individual (population-best pop)
                                     #:iterations (list-ref local-params 0)
                                     #:population-size (list-ref local-params 1)
                                     #:chromosome-length (list-ref local-params 2)
                                     #:mutation-rate (list-ref local-params 3)
                                     #:gene-size (list-ref local-params 4)
                                     #:elite (list-ref local-params 5)
                                     #:inversion-rate (list-ref local-params 6)
                                     #:evaluation (list-ref local-params 7)
                                     #:report (list-ref local-params 8)
                                     #:maxmin (list-ref local-params 9)
                                     #:range-size (list-ref local-params 10)
                                     #:range-offset (list-ref local-params 11)
                                     #:criteria (list-ref local-params 12))))
                                 (define (process-input)
                                   (let ((run (place-channel-get ch)))
                                     (if run (begin (run-iteration) (process-input)) '())))
                                 (process-input)))
                        (range parallel-count)))
           (pop-list (build-pop-list parallel-count))
           (island-list (map island pop-places pop-list))
           (parallel-cycle
            (lambda (i isl)
              (display i) (newline)
              (if (zero? i) 
                  (begin 
                    (map place-kill (map island-pl isl))
                    (map island-pop isl))
                  (parallel-cycle 
                   (- i 1) 
                   (do-cross-with-places (map (lambda (e)
                                                (let ((pla (island-pl e))
                                                      (dat (island-pop e)))
                                                  (place-channel-put pla #t)
                                                  (place-channel-put pla (list dat param-set))
                                                  (island pla (place-channel-get pla))))
                                              isl)))))))
    (parallel-cycle cycles island-list)))