(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (cons-all first rests)
  (cond
    ((null? rests) nil)
    (else (cons (append (cons first nil) (car rests)) (cons-all first (cdr rests))))
  )
)

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))

;; Returns a list of two-element lists
(define (enumerate s)
  (define (count i s)
    (cond ((null? s) nil)
          (else (cons (cons i (cons (car s) nil)) (count (+ i 1) (cdr s))))
      )
    )
  (count 0 s)
  )


  ;; List all ways to make change for TOTAL with DENOMS
  (define (list-change total denoms)
    (cond ((= total 0) nil)
          ((null? denoms) nil)
          ((> total (car denoms))
            (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
          ((= total (car denoms))
            (cons (list (car denoms)) (list-change total (cdr denoms))))
          (else (list-change total (cdr denoms))))
    )


;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           (cons (cons 'lambda 
                 (cons (car (zip (let-to-lambda values))) (let-to-lambda body)))
                 (cadr (zip (let-to-lambda values)))
            )
           ))
        (else
         (map let-to-lambda expr)
         )))
