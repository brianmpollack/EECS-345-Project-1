; Alex Lucas (mal175), Brian Pollack (bmp55)
; EECS 345 Project 1, Part 1


; Specifications:
;   state: ((x y ...)(1 5 ...))

(load "simpleParser.scm") ;load parser provided with project description

(define interpret
  (lambda (f)
    (state (parser f) '())))

(define state
  (lambda (r s)
    (cond
      ((null? r) s)
      ((eq? (car (car r)) 'var) (if (member? (cdr (car r)) (car s))
                                    (error 'variable\ already\ declared)
                                    (state (cdr r) (cons (cons (car (cdr (car r))) (car s))
                                                         (if (null? (cdr (cdr (car r))))
                                                             (cons (cons '() (car (cdr s))) '())
                                                             (cons (cons (car (cdr (cdr (car r)))) (car (cdr s))) '())))))) ;variable declaration
      ((eq? (car (car r)) '=) (if (not (member? (cdr (car r)) (car s)))
                                  (error 'variable\ not\ declared)
                                  
      ))))) 

(define assign
  (lambda (var val s)
    (cond
      ((eq? (car (car s)) var) (cons (car s) (cons (cons val (cdr (cdr s))) '())))
      (else ((lambda (blah)
                 (cons (cons (car (car s)) (car blah)) (cons (cons (car (cadr s)) (cadr blah)) '()) ) )
             (assign var val (cons (cdr (car s)) (cons (cdr (car (cdr s))) '()))))) )))
    
(define value ;Takes a rule and state and produces a numeric value
  (lambda (r s)
    (cond
      ((null? r)
       (error 'null\ value))
      ((number? r) r)
      (else (value (car r) s)))))


(define boolean ;Takes a rule and state and produces true/false
  (lambda (r s)
    ;((car r) car (cdr r) cdr (cdr r))))
    (cond
      ((equal? (car r) '<) (< (value (car (cdr r)) s) (value(cdr (cdr r)) s)))
      ((equal? (car r) '>) (> (value (car (cdr r)) s) (value(cdr (cdr r)) s)))
      ((equal? (car r) '==) (= (value (car (cdr r)) s) (value(cdr (cdr r)) s)))
      ((equal? (car r) '!=) (not (= (value (car (cdr r)) s) (value(cdr (cdr r)) s))))
      ((equal? (car r) '<=) (<= (value (car (cdr r)) s) (value(cdr (cdr r)) s)))
      ((equal? (car r) '>=) (>= (value (car (cdr r)) s) (value(cdr (cdr r)) s))))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else (or (eq? (car l) a) (member? a (cdr l)))))))
