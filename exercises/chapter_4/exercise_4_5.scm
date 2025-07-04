(define (cond? exp) (tagged-list exp 'cond))
(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) (car clause)))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (cond-recipient-clause? clause)
  (tagged-list (cond-actions clause) '=>))
(define (cond-recipient clause)
  (cadr (cond-actions clause)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (cond-recipient-clause? first)
                (make-if (cond-predicate first)
                         (list (cond-recipient first) (cond-predicate first))
                         'false)
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
