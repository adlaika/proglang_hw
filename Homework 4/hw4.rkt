
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (nats) ;nats stream for testing
  (define (thnk x)
    (cons x (lambda () (thnk (+ x 1)))))
  (thnk 1))

(define (powers-of-two) ;pows-of-2 stream for testing
  (define (thnk x)
    (cons x (lambda () (thnk (* x 2)))))
  (thnk 2))

; problem 1
(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

;problem 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (list-ref xs (remainder n (length xs)))]))

;problem 4
(define (stream-for-n-steps s n)
  (if (<= n 0) null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;problem 5
(define (funny-number-stream)
  (define (thnk x)
    (cons (if (= 0 (remainder x 5)) 
              (- x) 
              x)
          (lambda () (thnk (+ x 1)))))
  (thnk 1))

;problem 6
(define (dan-then-dog)
  (define (thnk x)
    (cons (if (even? x)
              "dan.jpg"
              "dog.jpg")
          (lambda () (thnk (+ x 1)))))
  (thnk 0))

;problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) 
                (cons (cons 0 (car (s))) 
                      (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

(define (stream-add-zero-with-defines s)
  (define (f s)
    (cons (cons 0 (car (s)))
          (lambda () (f (cdr (s))))))
  (lambda () (f s)))

;problem 8
(define (cycle-lists xs ys)
  (letrec 
      ([f (lambda (xs ys n) 
            (cons 
             (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
             (lambda () 
               (f xs ys (+ n 1)))))])
    (lambda () (f xs ys 0))))

;problem 9
(define testvec (vector (cons 2 1) (cons 3 1)))

(define (vector-assoc v vec)
  (define (f v vec pos)
    (if (< pos (vector-length vec))
        (if (and (pair? (vector-ref vec pos)) (equal? v (car(vector-ref vec pos))))
            (vector-ref vec pos)
            (f v vec (+ 1 pos)))
        #f))
  (f v vec 0))

;problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)] ;create empty cache of length n
           [pos 0] ;set initial cache write position to 0
           [f (lambda (v)
                (cond [(vector-assoc v cache) (vector-assoc v cache)] ;if answer's in cache, return it
                      [#t (begin ;otherwise, write to cache position and increment pos
                            (vector-set! cache pos (assoc v xs))
                            (set! pos (remainder (+ pos 1) n))
                            (assoc v xs))]
                      ))])
    f))