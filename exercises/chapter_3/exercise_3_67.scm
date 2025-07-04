(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
    (interleave
     (stream-map (lambda (y) (list (stream-car s) y))
                 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t))))))
