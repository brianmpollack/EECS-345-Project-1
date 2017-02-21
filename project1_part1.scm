; Alex Lucas (mal175), Brian Pollack (bmp55)
; EECS 345 Project 1, Part 1


; Specifications:
;   state: ((x y ...)(1 5 ...))

(load "simpleParser.scm") ;load parser provided with project description

(define interpret
  (lambda (file)
    (state (parser file) '(()())))) ;Initialize empty state

(define state
  (lambda (parsetree instate)
    (cond
      ((null? parsetree) instate) ;If we finished the parse tree, we are done
      ((eq? (first_variable_in_list parsetree) 'var) (if (member? (second (car parsetree)) (car instate)) ;Variable assignment
                                            (error 'variable\ already\ declared)
                                            (state (cdr parsetree) (cons (cons (cadr (car parsetree)) (car instate))
                                                                         (if (null? (cdr (cdr (car parsetree))))
                                                                             (cons (cons '() (car (cdr instate))) '())
                                                                             (cons (cons (value* (third (car parsetree)) instate) (cadr instate)) '())))))) ;variable declaration
      ((eq? (first_variable_in_list parsetree) '=) (if (not (member? (cadr (car parsetree)) (car instate))) ;Variable assignment
                                          (error 'variable\ not\ declared)
                                          (state (cdr parsetree) (assign* (cadr (car parsetree)) (cadr (cdr (car parsetree))) instate))))
      ((eq? (first_variable_in_list parsetree) 'return) ((lambda (returnval) ;return statement
                                              (cond
                                                ((not (boolean? returnval)) returnval)
                                                (else (if returnval 'true 'false)
                                                      )))


                                            (value* (second (car parsetree)) instate)))

      ((eq? (first_variable_in_list parsetree) 'if) ((lambda (newstate newcondition) ;if statement
                                          (if (boolean newcondition newstate)
                                              (state (cons (third (car parsetree)) (cdr parsetree)) newstate)
                                              (if (fourth? (car parsetree))
                                                  (state (cons (fourth (car parsetree)) (cdr parsetree)) newstate)
                                                  (state (cdr parsetree) instate))))
                                        (newstate (cadr (car parsetree)) instate) (newcondition (second (car parsetree)))))
      ((eq? (first_variable_in_list parsetree) 'while) ((lambda (newstate newcondition) ;while loop
                                             (if (boolean newcondition newstate)
                                                 (state (cons (third (car parsetree)) parsetree) instate)
                                                 (state (cdr parsetree) instate)
                                                 ))
                                           (newstate (cadr (car parsetree)) instate) (newcondition (second (car parsetree)))))
      )))

(define assign* ;Recursively assigns variables in the state and returns the state
  (lambda (variable expression state) 
    (cond
      ((and (pair? expression) (eq? (car expression) '=)) (assign* variable (second expression) (assign* (second expression) (third expression) state)))
      ((eq? (first_state_variable state) variable)
       (assign_variable variable expression state))
      
      (else ((lambda (memoization_variable)
               (cons (cons (car (car state)) (car memoization_variable)) (cons (cons (first_state_value state) (cadr memoization_variable)) '()) ) )
             (assign* variable (value* expression state) (state_cdr state))))

      )))
      

(define assign_variable ;Set the variable in the state to the given value
  (lambda (variable expression state)
    (cons (car state) (cons (cons (value* expression state) (cdr (cadr state)) )'()))
    ))

(define first_variable_in_list ;Returns the first variable in a list
  (lambda (list)
    (car (car list))))
;These are the same and accomplish the same thing, but it helps to understand the code in other areas of this project
(define first_state_variable ;Returns the first variable name in the state binding
  (lambda (state)
    (first_variable_in_list state)))

(define first_state_value ;Returns the first value in the state binding
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
      ((symbol? expression) (cond
                              ((eq? expression 'true) #t)
                              ((eq? expression 'false) #f)
                              (else
                               (value_for_variable expression state))))
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

(define value_for_variable ;Gets the value for a variable in the given state. Returns the value
  (lambda (variable state)
    (cond
      ((not (member? variable (car state))) (error 'variable\ not\ declared))
      ((eq? (first_state_variable state) variable) (if (null? (first_state_value state)) (error 'variable\ not\ defined) (first_state_value state))) ;If value is not yet defined, throw error. Else, return value.
      (else (value_for_variable variable (state_cdr state)))
      )))

(define newstate ;Creates a new state defined by the current state and the condition
  (lambda (condition state)
    (cond
      ((and (not (pair? (second condition))) (not (pair? (third condition)))) state)
      ((and (not (pair? (third condition))) (pair? (second condition)) (eq? (car (second condition)) '=)) (assign* (second (second condition)) (third (second condition)) state))
      ((and (not (pair? (second condition))) (pair? (third condition)) (eq? (car (third condition)) '=)) (assign* (second (third condition)) (third (third condition)) state))
      ((and (pair? (second condition)) (pair? (third condition)) (eq? (car (second condition)) '=) (eq? (car (third condition)) '=))
       (assign* (second (third condition)) (third (third condition)) (assign* (second (second condition)) (third (second condition)) state)))
      (else state)
      )))

(define newcondition ;Creates a new condition based on the condition. Useful for nested expressions (replace the computed value with the variable) - used in conjunction with newstate.
  (lambda (condition)
    (cond
      ((and (not (pair? (second condition))) (not (pair? (third condition)))) condition)
      ((and (not (pair? (third condition))) (pair? (second condition)) (eq? (car (second condition)) '=)) (consthree (car condition) (second (second condition)) (third condition)))
      ((and (not (pair? (second condition))) (pair? (third condition)) (eq? (car (third condition)) '=)) (consthree (car condition) (second condition) (second (third condition))))
      ((and (pair? (second condition)) (pair? (third condition)) (eq? (car (second condition)) '=) (eq? (car (third condition)) '=))
       (consthree (car condition) (second (second condition)) (second (third condition))))
      (else condition)
      )))

(define consthree ;Cons three items 
  (lambda (first second third)
    (cons first (cons second (cons third '())))))

(define boolean ;Takes a rule and state and produces true/false
  (lambda (parsetree instate)
    (cond
      ((equal? (car parsetree) '<) (< (value* (second parsetree) instate) (value*(third parsetree) instate)))
      ((equal? (car parsetree) '>) (> (value* (second parsetree) instate) (value*(third parsetree) instate)))
      ((equal? (car parsetree) '==) (= (value* (second parsetree) instate) (value*(third parsetree) instate)))
      ((equal? (car parsetree) '!=) (not (= (value* (second parsetree) instate) (value*(third parsetree) instate))))
      ((equal? (car parsetree) '<=) (<= (value* (second parsetree) instate) (value*(third parsetree) instate)))
      ((equal? (car parsetree) '>=) (>= (value* (second parsetree) instate) (value*(third parsetree) instate)))
      ((equal? (car parsetree) '&&) (and (value* (second parsetree) instate) (value* (third parsetree) instate)))
      ((equal? (car parsetree) '||) (or (value* (second parsetree) instate) (value* (third parsetree) instate)))
      ((equal? (car parsetree) '!) (not (value* (second parsetree) instate)))
      )))

(define member? ;Returns #t if list contains atom
  (lambda (atom list)
    (cond
      ((null? list) #f)
      (else (or (eq? (car list) atom) (member? atom (cdr list)))))))


(define third? ;Returns #t if list contains a third item
  (lambda (list)
    (not (null?  (cdr (cdr list))))))

(define fourth? ;Returns #t if list contains a fourth item
  (lambda (list)
    (not (null? (cdr (cdr (cdr list)))))))

