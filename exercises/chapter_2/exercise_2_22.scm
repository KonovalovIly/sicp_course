(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        (reverse answer)
        (iter (cdr things) (cons (square (car things)) answer))))
  (iter items '()))
