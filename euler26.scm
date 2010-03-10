; least significant digit first
; sign is attached to every digit,
(define (digits n) (if (>= (abs n) 10) (cons (remainder n 10) (digits (quotient n 10))) (list n) ))

(define (undigit l) (if (null? l) 0 (+ (car l) (* 10 (undigit (cdr l))))))

;(define-struct div quot rem rec)
(define (division n k) 
  (letrec ((schriftlich-rest (lambda (l k) (let ((n (car l)))
                            ; assumes (quotient n k) -> 0
                            (cond
                              ((zero? (remainder (* n 10) k)) (list (list (quotient (* n 10) k)) 0))
                              ((member (remainder (* n 10) k) l)
                               (list (list (quotient (* n 10) k)) (+ 1 (- (length l) (length (member (remainder (* n 10) k) l))))))
                               ;(list (list (quotient (* n 10) k)) (list l (member (remainder (* n 10) k) l))))
                              (else (let ((v (schriftlich-rest  (cons (remainder (* n 10) k) l) k))) 
                                    (list (cons (quotient (* n 10) k) (car v)) (cadr v))))
                              )
                            ))))
  (cons (quotient n k)
                             (schriftlich-rest (list (remainder n k)) k)
                             )
  ))

(define (main n) (let loop ((d n) (maxd 1) (maxr 0))
  (if (< d maxr) (print "Max. Recurrence: " maxr " at d = " maxd #\newline)
    (let ((r (caddr (division 1 d))))
      (if (> r maxr)
        (loop (- d 1) d r)
        (loop (- d 1) maxd maxr))))))

(main 1000)
