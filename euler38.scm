; least significant digit first
; sign is attached to every digit,
(define (digits n) (if (>= (abs n) 10) (cons (remainder n 10) (digits (quotient n 10))) (list n) ))

(define (undigit l) (if (null? l) 0 (+ (car l) (* 10 (undigit (cdr l))))))

; http://stackoverflow.com/questions/387775/using-and-with-the-apply-function-in-scheme
(define list-and (lambda (args) (if (null? args) #t (and (car args) (list-and (cdr args))))))

(define (print . l) (for-each display l))


(define (pandigital? nn) (let ((to-be-found (vector #t #f #f #f #f #f #f #f #f #f)))
                          (if (= 9 (length nn))
                              (begin (map (lambda (k) (vector-set! to-be-found k #t)) nn)
                                  (list-and (vector->list to-be-found)))
                              #f)
    ))


(define (concat-prod k n)
  (if (> n 0)
      (append (digits (* k n)) (concat-prod k (- n 1)))
      '()))

(define (euler38)
  (apply max (map undigit 
    (let outer-loop ((k 99999) (acc '()))
      (if (zero? k)
        acc
        (outer-loop (- k 1)
            (let inner-loop ((n 9) (accc acc))
              (let ((nn (concat-prod k n)))
              (if (zero? n)
                accc
                (if (pandigital? nn)
                  (inner-loop (- n 1) (cons nn accc))
                  (inner-loop (- n 1) accc)))
              )
     ))))
    )))

(display (euler38))
(newline)
