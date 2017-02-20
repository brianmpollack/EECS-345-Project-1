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
      ((eq? (car (car r)) 'var) (if (member? (second (car r)) (car s))
                                    (error 'variable\ already\ declared)
                                    (state (cdr r) (cons (cons (cadr (car r)) (car s))
                                                         (if (null? (cdr (cdr (car r))))
                                                             (cons (cons '() (car (cdr s))) '())
                                                             (cons (cons (value* (third (car r)) s) (cadr s)) '())))))) ;variable declaration
      ((eq? (car (car r)) '=) (if (not (member? (cadr (car r)) (car s)))
                                  (error 'variable\ not\ declared)
                                  (state (cdr r) (assign* (cadr (car r)) (cadr (cdr (car r))) s))))
      ((eq? (car (car r)) 'return) ((lambda (returnval)
                                      (cond
                                        ((not (boolean? returnval)) returnval)
                                        (else (if returnval 'true 'false)
                                            )))


                                      (value* (second (car r)) s)))

      ((eq? (car (car r)) 'if) ((lambda (newstate newcondition)
                                  (if (boolean newcondition newstate)
                                      (state (cons (third (car r)) (cdr r)) newstate)
                                      (if (fourth? (car r))
                                          (state (cons (fourth (car r)) (cdr r)) newstate)
                                          (state (cdr r) s))))
                                (newstate (cadr (car r)) s) (newcondition (second (car r)))))
      ((eq? (car (car r)) 'while) ((lambda (newstate newcondition)
                                     (if (boolean newcondition newstate)
                                         (state (cons (third (car r)) r) s)
                                         (state (cdr r) s)
                                       ))
                                   (newstate (cadr (car r)) s) (newcondition (second (car r)))))
      )))

      (define assign*
        (lambda (variable expression state)
          (cond
            ((and (pair? expression) (eq? (car expression) '=)) (assign* variable (second expression) (assign* (second expression) (third expression) state)))
            ((eq? (first_state_variable state) variable)
             (assign_variable variable expression state))
      
            (else ((lambda (memoization_variable)
                     (cons (cons (car (car state)) (car memoization_variable)) (cons (cons (first_state_value state) (cadr memoization_variable)) '()) ) )
                   (assign* variable (value* expression state) (state_cdr state))))

            )))
      

      (define assign_variable
        (lambda (variable expression state)
          (cons (car state) (cons (cons (value* expression state) (cdr (cadr state)) )'()))
          ))

      (define first_state_variable
        (lambda (state)
          (car (car state))))

      (define first_state_value
        (lambda (state)
          (car (cadr state))))

      (define state_cdr ;Returns the state without the first variable assignment (without the first elements of the binding lists)
        (lambda (state)
          (cons (cdr (car state)) (cons (cdr (cadr state)) '()))))
    
      (define value* ;Takes a rule and state and produces a value
        (lambda (expression state)
          (cond
            ((null? expression)
             (error 'null\ expression))
            ((boolean? expression) expression)
            ((number? expression) expression)
            ;((symbol? r) (if (eq? (car (car s)) r) (car (cadr s)) (value r (cons (cdar s) (cons (cdr (cadr s)) '())))))
            ((symbol? expression) (cond
                                    ((eq? expression 'true) #t)
                                    ((eq? expression 'false) #f)
                                    (else
                                      (value_for_variable expression state))))
            ;((eq? (car expression '=)
            (else (if (third? expression) (operation (consthree (car expression) (value* (second expression) state) (value* (third expression) state)) state)
                      (operation (cons (car expression) (cons (value* (second expression) state) '())) state)))
            )))

      (define operation ; + - * / % (supports unary -)
        (lambda (expression state)
          (cond
            ((equal? (car expression) '+) (+ (value* (second expression) state) (value* (third expression) state)))
            ((equal? (car expression) '-) (if (third? expression)
                                              (- (value* (second expression) state) (value* (third expression) state))
                                              (- (value* (second expression) state))))
            ((equal? (car expression) '*) (* (value* (second expression) state) (value* (third expression) state)))
            ((equal? (car expression) '/) (truncate (/ (value* (second expression) state) (value* (third expression) state))))
            ((equal? (car expression) '%) (modulo (value* (second expression) state) (value* (third expression) state)))
            (else (boolean expression state))

            )))

      (define value_for_variable
        (lambda (variable state)
          (cond
            ((null? (car state)) (error 'variable\ not\ defined))
            ((eq? (car (car state)) variable) (car (cadr state)))
            (else (value_for_variable variable (state_cdr state)))
            )))

      (define newstate
        (lambda (condition state)
          (cond
            ((and (not (pair? (second condition))) (not (pair? (third condition)))) state)
            ((and (not (pair? (third condition))) (pair? (second condition)) (eq? (car (second condition)) '=)) (assign* (second (second condition)) (third (second condition)) state))
            ((and (not (pair? (second condition))) (pair? (third condition)) (eq? (car (third condition)) '=)) (assign* (second (third condition)) (third (third condition)) state))
            ((and (pair? (second condition)) (pair? (third condition)) (eq? (car (second condition)) '=) (eq? (car (third condition)) '=))
             (assign* (second (third condition)) (third (third condition)) (assign* (second (second condition)) (third (second condition)) state)))
            (else state)


            ;(if (and (pair? (third condition)) (eq? (car (third condition)) '=)) (assign* (third (third condition)) (third (third condition)) state) ())
            )))

      (define newcondition
        (lambda (condition)
          (cond
            ((and (not (pair? (second condition))) (not (pair? (third condition)))) condition)
            ((and (not (pair? (third condition))) (pair? (second condition)) (eq? (car (second condition)) '=)) (consthree (car condition) (second (second condition)) (third condition)))
            ((and (not (pair? (second condition))) (pair? (third condition)) (eq? (car (third condition)) '=)) (consthree (car condition) (second condition) (second (third condition))))
            ((and (pair? (second condition)) (pair? (third condition)) (eq? (car (second condition)) '=) (eq? (car (third condition)) '=))
             (consthree (car condition) (second (second condition)) (second (third condition))))
            (else condition)
            )))

      (define consthree
        (lambda (first second third)
          (cons first (cons second (cons third '())))))
      (define boolean ;Takes a rule and state and produces true/false
        (lambda (r s)
          ;((car r) car (cdr r) cdr (cdr r))))
          (cond
            ((equal? (car r) '<) (< (value* (car (cdr r)) s) (value*(cadr (cdr r)) s)))
            ((equal? (car r) '>) (> (value* (car (cdr r)) s) (value*(cadr (cdr r)) s)))
            ((equal? (car r) '==) (= (value* (car (cdr r)) s) (value*(cadr (cdr r)) s)))
            ((equal? (car r) '!=) (not (= (value* (car (cdr r)) s) (value*(cadr (cdr r)) s))))
            ((equal? (car r) '<=) (<= (value* (car (cdr r)) s) (value*(cadr (cdr r)) s)))
            ((equal? (car r) '>=) (>= (value* (car (cdr r)) s) (value*(cadr (cdr r)) s)))
            ((equal? (car r) '&&) (and (value* (second r) s) (value* (third r) s)))
            ((equal? (car r) '||) (or (value* (second r) s) (value* (third r) s)))
            ((equal? (car r) '!) (not (value* (second r) s)))

            )))
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


      