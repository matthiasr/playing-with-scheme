; assumes l is ordered
(define (ordered-insert v l) (cond ((null? l) (list v))
                                   ((> v (car l)) (cons (car l) (ordered-insert v (cdr l))))
                                   (else (cons v l))))

;(define (last l) (list-ref l (- (length l) 1)))
;(define (without-last l) (if (null? (cdr l))
;                           (list)
;                           (cons (car l) (without-last (cdr l)))))

(define (without v l) (cond
                        ((null? l) l)
                        ((eq? v (car l)) (without v (cdr l)))
                        (else (cons (car l) (without v (cdr l))))))


(define (next> v l) (cond
                      ((null? l) #f)
                      ((< v (car l)) (car l))
                      (else (next> v (cdr l)))))

(define (next-perm l) (if (or (null? l) (null? (cdr l)))
                        l
                        (let ((ll (next-perm (cdr l))))
                          (if (apply < ll)
                            (let ((nn (next> (car l) ll)))
                              (if nn
                                (cons nn (ordered-insert (car l) (without nn ll)))
                                (ordered-insert (car l) ll) ; will end up last under these conditions
                                ))
                            (cons (car l) ll)))))
                            
(define (perm l n) (if (= n 0) l (perm (next-perm l) (- n 1))))

; Project Euler #24
(print (perm '(0 1 2 3 4 5 6 7 8 9) 999999) #\newline)
