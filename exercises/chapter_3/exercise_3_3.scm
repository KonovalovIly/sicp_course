(
    define (make-account balance password)
        (define (withdraw amount)
            (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
        (define (deposit amount) (set! balance (+ balance amount)) balance)
        (define (dispatch pass m)
        (if (eq? password pass)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request: MAKE-ACCOUNT" m)))
                "Incorrect password"
            )
        )
        dispatch
)
