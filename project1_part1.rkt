; Alex Lucas (mal175), Brian Pollack (bmp55)
; EECS 345 Project 1, Part 1


; Specifications:
;   state: ((x y ...)(1 5 ...))

(load "simpleParser.scm") ;load parser provided with project description

(define interpret
  (lambda (f)
    (state (parser f) '(()()))))

(define state
  (lambda (r s)
    (cond
      ((null? r) s)
      ((eq? (car (car r)) 'var) (if (member? (cdr (car r)) (car s))
                                    (error 'variable\ already\ declared)
                                    (state (cdr r) (cons (cons (cadr (car r)) (car s))
                                                         (if (null? (cdr (cdr (car r))))
                                                             (cons (cons '() (car (cdr s))) '())
                                                             (cons (cons (cadr (cdr (car r))) (cadr s)) '())))))) ;variable declaration
      ((eq? (car (car r)) '=) (if (not (member? (cadr (car r)) (car s)))
                                  (error 'variable\ not\ declared)
                                  (assign (cadr (car r)) (cadr (cdr (car r))) s)))
      ((eq? (car (car r)) 'return) (value (cdar r) s))

      ((eq? (car (car r)) 'if) (if (boolean (cadr (car r)) s)
                                   (state (cdr (cdr (car r))) s)
                                   (if (not (null? (cdr (cdr (cdr (car r))))))
                                       (state (cdr (cdr (cdr (car r)))) s)
                                       ())))
     ; ((eq? (car (car r)) 'while) (if (boolean (cadr (car r)) s)
      ;                                (state 


      )))
      
(define assign
  (lambda (var expression state)
    (cond
      ((eq? (car (car state)) var) (cons (car state) (cons (cons (value expression state) (cadr state)) '())))
      (else ((lambda (blah)
                 (cons (cons (car (car state)) (car blah)) (cons (cons (car (cadr state)) (cadr blah)) '()) ) )
             (assign var (value expression state) (cons (cdr (car state)) (cons (cdr (car (cdr state))) '()))))) )))
    
(define value ;Takes a rule and state and produces a numeric value
  (lambda (expression state)
    (cond
      ((null? expression)
       (error 'null\ expression))
      ((number? expression) expression)
      ;((symbol? r) (if (eq? (car (car s)) r) (car (cadr s)) (value r (cons (cdar s) (cons (cdr (cadr s)) '())))))
      ((symbol? expression) (value_for_variable expression state))
      (else (operation expression state))
            )))

(define operation ; + - * / % (supports unary -)
  (lambda (expression state)
    (cond
      ((equal? (car expression) '+) (+ (value (second expression) state) (value (third expression) state)))
      ((equal? (car expression) '-) (- (value (second expression) state) (value (third expression) state)))
      ((equal? (car expression) '*) (* (value (second expression) state) (value (third expression) state)))
      ((equal? (car expression) '/) (/ (value (second expression) state) (value (third expression) state)))
      ((equal? (car expression) '%) (modulo (value (second expression) state) (value (third expression) state)))
      )))

(define value_for_variable
  (lambda (variable state)
    (cond
      ((null? (car state)) (error 'variable\ not\ defined))
      ((eq? (car (car state)) variable) (car (cadr state)))
      (else (value_for_variable variable (cons (cdr (car state)) (cons (cdr (cadr state)) '()))))
      )))

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


(define third?
  (lambda (l)
    (not (null?  (cdr (cdr l))))))

(define fourth?
  (lambda (l)
    (not (null? (cdr (cdr (cdr l)))))))


