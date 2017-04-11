; Alex Lucas (mal175), Brian Pollack (bmp55)
; EECS 345 Project 1, Part 1


; Specifications:
;   state: ((x y ...)(1 5 ...))

(load "functionParser.scm") ;load parser provided with project description
(require racket/trace)

(define interpret
  (lambda (file)
    (state (parser file) new_state '() '(()) #f (lambda (v) v)))) ;Initialize empty state

(define new_state '((()())))
(define new_layer '(()()))
(define new_func_layer '())

(define addlayer
  (lambda (state)
    (cons new_layer state)))
(define addfunctionlayer
  (lambda (functions)
    (cons new_func_layer functions)))

(define strip_first_command cdr)
(define top_level_state car)
(define lower_level_states cdr)
(define return_val
  (lambda (parsetree)
    (second (car parsetree))))
(define if_body third)
(define else_body fourth)
(define if_condition second)
(define while_condition second)
(define while_body third)
(define begin_statements cdr)

(define state
  (lambda (parsetree instate err functions main return) 
    (cond
     
      ((not (null? err)) (cond
                             ((null? parsetree) (raise err))
                             ((not (eq? (next_command parsetree) 'catch)) (state (strip_first_command parsetree) instate err functions main (lambda (v) (return v))))
                             (else (state (cddr parsetree) (add_var_with_value (caadr parsetree) err instate) '() functions main (lambda (v) (return v))))))
      ((null? parsetree) (if (is_state instate)
                             (cons '() instate)
                             instate)) ;If we finished the parse tree, we are done
      ((eq? (next_command parsetree) 'var) (if (value? (next_line parsetree))
                                               ((lambda (value)
                                               (if (list? value)
                                                  (state (strip_first_command parsetree) (add_var_with_value (var (next_line parsetree)) (car value) (cddr value)) err functions main (lambda (v) (return v)))
                                                  (state (strip_first_command parsetree) (add_var_with_value (var (next_line parsetree)) (value* (val (next_line parsetree)) instate (cddr (car parsetree)) functions main) instate) err functions main (lambda (v) (return v)))
                                              )) (value* (val (next_line parsetree)) instate (cddr (car parsetree)) functions main))
                                             (state (strip_first_command parsetree) (cons (add_var (var (next_line parsetree)) (top_level_state instate)) (lower_level_states instate) ) err functions main (lambda (v) (return v)))))
                                             
     
      ;variable declaration
      ((eq? (next_command parsetree) '=) 
                                                       ((lambda (value)
                                                          (if (list? value)
                                                              (state (strip_first_command parsetree) (assign (var (next_line parsetree)) (car value) (cddr value)(cddr value) parsetree functions main) err functions main (lambda (v) (return v)))
                                                              (state (strip_first_command parsetree) (assign (var (next_line parsetree)) value instate instate parsetree functions main) err functions main (lambda (v) (return v))))
                                                          )(value* (val (next_line parsetree)) instate (cddr (car parsetree)) functions main)))
                                                       
                                                      

      ((eq? (next_command parsetree) 'return) (if main
                                                  ((lambda (returnval) ;return statement
                                                     (if (list? returnval)
                                                         (car returnval)
                                                         
                                                           (cond
                                                             ((not (boolean? returnval)) returnval)
                                                             (else (if returnval 'true 'false)
                                                                   ))))


                                                         (value* (return_val parsetree)  (if (is_state instate)
                                                                                                             instate
                                                                                                              (cdr instate)) (cdr (car parsetree)) functions main))
                                                  (cons (value* (return_val parsetree) instate parsetree functions main) (if (end_in_parsetree parsetree)
                                                                                                                             (cdr instate)
                                                                                                                             instate)))) 

      ((eq? (next_command parsetree) 'if) ((lambda (newstate newcondition) ;if statement
                                                       (if (boolean newcondition newstate)
                                                           (state (cons (if_body (next_line parsetree)) (strip_first_command parsetree)) newstate err functions main (lambda (v) (return v)))
                                                           (if (fourth? (next_line parsetree))
                                                               (state (cons (else_body (next_line parsetree)) (strip_first_command parsetree)) newstate err functions main (lambda (v) (return v)))
                                                               (state (strip_first_command parsetree) instate err functions main (lambda (v) (return v))))))
                                                     (newstate (if_condition (next_line parsetree)) instate) (newcondition (if_condition (next_line parsetree)))))
      
      ((eq? (next_command parsetree) 'while) ((lambda (newstate newcondition) ;while loop
                                                          (if (boolean newcondition newstate)
                                                              (state (cons (while_body (next_line parsetree)) parsetree) instate err functions main (lambda (v) (return v)))
                                                              (state (strip_first_command parsetree) instate err functions main (lambda (v) (return v)))
                                                              ))
                                                        (newstate (while_condition (next_line parsetree)) instate) (newcondition (while_condition (next_line parsetree)))))
      ((eq? (next_command parsetree) 'begin) (state (bracket_cons (reverse (begin_statements (next_line parsetree))) (parse_with_end parsetree)) (addlayer instate) err functions main (lambda (v) (return v))))
      ((eq? (next_command parsetree) 'end) (state (strip_first_command parsetree) (lower_level_states instate) err functions main (lambda (v) (return v))))
      ((eq? (next_command parsetree) 'break) (if (can_break? instate)
                                                           (state (pop_through_while parsetree) (lower_level_states instate) err functions main (lambda (v) (return v)))
                                                           (error 'cannot\ break)))
      ((eq? (next_command parsetree) 'continue) (state (pop_to_while parsetree) (lower_level_states instate) err functions main (lambda (v) (return v))))
      ((eq? (next_command parsetree) 'throw) (state (strip_first_command parsetree) instate (value* (second (car parsetree)) instate (cddr (car parsetree)) functions main) functions main (lambda (v) (return v))))
      ((eq? (next_command parsetree) 'try) (state (new_tcf_parsetree parsetree) instate err functions main (lambda (v) (return v))))
      ((eq? (next_command parsetree) 'catch) (state (pop_catch_block parsetree) instate err functions main (lambda (v) (return v))))
      ((eq? (next_command parsetree) 'catch_end) (state (strip_first_command parsetree) instate err functions main (lambda (v) (return v))))
      ((eq? (next_command parsetree) 'function) (cond
                                                  ((and (is_main? (car parsetree)) (not (null? (caar instate)))) (state (fourth (car parsetree)) (addlayer instate) err functions #t (lambda (v) (return v))))
                                                  ((is_main? (car parsetree))(state (fourth (car parsetree)) instate err functions #t (lambda (v) (return v))))  
                                                  (else (state (strip_first_command parsetree) instate err (add_function (car parsetree) functions) main (lambda (v) (return v))))))
      ((eq? (next_command parsetree) 'funcall) ((lambda (func)
                                                  ((lambda (new_functions)
                                                  ((lambda (statecall)
                                                     
                                                    
                                                         (state (strip_first_command parsetree) statecall err functions main (lambda (v) (return v) )))
                                                     
                                                      (state (remove_functions (fourth func))
                                                         (define_args (third func) (cddr (car parsetree)) (if (is_state instate)
                                                                                                              (addlayer instate)
                                                                                                              (addlayer (cdr instate)))parsetree functions main (function_in_scope? (function_name func) (cadr new_functions)))
                                                        

                                                      err
                                                      new_functions
                                                      #f
                                                      (lambda (v) (return v))))
                                                     ) (get_functions (fourth func) (addfunctionlayer functions))))
                                                (get_function (second (car parsetree)) functions)))
      )))

(define end_in_parsetree
  (lambda (parsetree)
    (cond
    ((null? parsetree) #f)
    ((eq? (next_command parsetree) 'end) #t)
    (else (end_in_parsetree (cdr parsetree))))))
(define is_state
  (lambda (state)
    (cond
      ((null? (car state)) #f)
      ((not (list? (car state))) #f)
      ((not (list? (car (car state)))) #f)
      ((not (list? (cadr (car state)))) #f)
      ((third? (car state)) #f)
      (else #t))))

(define function_in_scope?
  (lambda (name function_scope)
    (cond
      ((null? function_scope) #f)
      (else (or (eq? name (second (car function_scope))) (function_in_scope name (cdr function_scope)))))))
    
(define function_name
  (lambda (function)
    (second function)))
     
(define define_args
  (lambda (vars vals state parsetree functions main in_scope)
    (cond
      ((and (null? vars) (not (null? vals))) (error 'too\ many\ arguments))
      ((and (null? vars) (or (null? (cdr state)) (null? (cddr state)))) state)
      ((and (not in_scope) (null? vars)) (cons (car state) (cddr state)))
      
      ((null? vars) state)
      ((null? vals) (error 'not\ enough\ arguments))
      (else (define_args (cdr vars) (cdr vals) (add_var_with_value (car vars) (value* (car vals) (cdr state) parsetree functions main) state) parsetree functions main in_scope)))))

  
(define last
  (lambda (l)
    (cond
    ((null?(cdr l)) (car l))
    (else (last (cdr l))))))
(define remove_functions
  (lambda (commands)
    (cond
    ((null? commands) '())
    ((eq? (next_command commands) 'function) (cdr commands))
    (else (cons (car commands) (remove_functions (cdr commands)))))))

(define get_functions
 (lambda (commands functions)
   (cond
   ((null? commands) functions)
   ((eq? (next_command commands) 'function) (cons (cons (car commands) (car functions)) (cdr functions)))
   (else (get_functions (cdr commands) functions)))))

(define vars_declared
  (lambda (vars instate)
    (cond
      ((null? vars) #f)
      ((var_declared (car vars) instate) #t)
      (else (vars_declared (cdr vars) instate)))))

(define function_in_scope
  (lambda (name scope)
    (cond
      ((null? scope) #f)
      ((eq? name (second (car scope))) #t)
      (else (function_in_scope name (cdr scope))))))
(define get_function
  (lambda (name functions)
    (cond
      ((null? functions) (error 'function\ not\ declared))
      ((null? (car functions)) (get_function name (cdr functions)))
      ((eq? name (second (car (car functions)))) (car (car functions)))
      (else (get_function name (cons (cdr (car functions)) (cdr functions)))))))
(define get_function_in_scope
  (lambda (name functions)
    (cond
      ((null? functions) (error 'no\ function))
      ((eq? (second (car functions)) name) (car functions))
      (else (get_function name (cdr functions))))))

(define is_main?
  (lambda (command)
    (eq? (second command) 'main)))

  (define pop_catch_block
  (lambda (parsetree)
    (cond
      ((eq? (next_command parsetree) 'catch_end) (cdr parsetree))
      (else (pop_catch_block (cdr parsetree))))))

(define add_function
  (lambda (newfunctions function)
    (cons (cons newfunctions (car function)) (cdr function))))

;extracts try catch finally statemetns and adds catch_end for easy popping
(define new_tcf_parsetree
  (lambda (parsetree)
    (cond
      ((catch_and_finally? parsetree) (append (append (append (append (second (car parsetree)) (extract_body (third (car parsetree)))) '(catch_end)) (cadr (fourth (car parsetree)))) (cdr parsetree)))
      ((catch_no_finally? parsetree) (append (append (append (second (car parsetree)) (extract_body (third (car parsetree)))) '(catch_end)) (cdr parsetree)))
      ((no_catch_finally? parsetree) (append (append (second (car parsetree)) (cadr (fourth (car parsetree)))) (cdr parsetree)))
      (else (append (second (car parsetree) (cdr parsetree)))))))

(define catch_and_finally?
  (lambda (parsetree)
    (and (not (null? (third (car parsetree)))) (not (null? (fourth (car parsetree)))))))
(define catch_no_finally?
  (lambda (parsetree)
    (and (not (null? (third (car parsetree)))) (null? (fourth (car parsetree))))))
(define no_catch_finally?
  (lambda (parsetree)
    (and (null? (third (car parsetree))) (not (null? (fourth (car parsetree)))))))

;(catch (e) (body)) -> (catch (e) body)
(define extract_body
  (lambda (block)
    (cons (car block) (cons (cadr block) (third block)))))
    

(define can_break?
  (lambda (state)
    (not (null? (cdr state)))))
(define parse_with_end
  (lambda (parsetree)
    (cons '(end) (cdr parsetree))))
(define next_line
  (lambda (parsetree)
    (car parsetree)))


(define var_declared
  (lambda (var instate)
    (cond
      ((null? instate) #f)
      ((member? var (variables (car instate))) #t)
      (else (var_declared var (cdr instate))))))

(define variables car)



(define add_var
  (lambda (var local)
    (cons (cons var (car local)) (cons (cons '() (cadr local)) '()))))

(define add_var_with_value
  (lambda (var value state)
    (cons (cons (cons var (car (car state))) (cons (cons (value* value state '() '() '()) (cadr (car state))) '())) (cdr state))))

(define var
  (lambda (line)
    (second line)))

(define val
  (lambda (line)
    (third line)))


(define assign
  (lambda (var expr instate full_state parsetree functions main)
    (cond
      ((null? instate) (error 'variable\ not\ declared))
      ((member? var (top_variables instate)) (cons (assign_to_layer var (value* expr full_state parsetree functions main) (car instate)) (cdr instate)))
      (else ((lambda (return)
               (cons (car instate) return))

             (assign var expr (cdr instate) full_state parsetree functions main))))))

(define assign_to_layer
  (lambda (var val top)
    (cond
      ((eq? (car (car top)) var) (replace_first_val val top))
      (else ((lambda (return)
               (cons (cons (car (car top)) (car return)) (cons (cons (first_state_value top) (cadr return)) '())))
             (assign_to_layer var val (state_cdr top)))))))

(define replace_first_val
  (lambda (expr state)
    (cons (car state) (cons (cons expr (cdr (cadr state))) '()))))

(define top_variables caar)

(define pop_through_catch
  (lambda (parsetree)
    (cond
      ((null? parsetree) (error 'error\ not\ caught))
      ((eq? (next_command parsetree) 'catch) (cdr parsetree))
      (else (pop_through_catch (cdr parsetree))))))

(define pop_through_while
  (lambda (parsetree)
    (cond
      ((eq? (next_command parsetree) 'while) (cdr parsetree))
      (else (pop_through_while (cdr parsetree))))))

(define pop_to_while
  (lambda (parsetree)
    (cond
      ((eq? (next_command parsetree) 'while) parsetree)
      (else (pop_to_while (cdr parsetree))))))

(define bracket_cons
  (lambda (reverse parsetree)
    (cond
      ((null? reverse) parsetree)
      (else (bracket_cons (cdr reverse) (cons (car reverse) parsetree))))))

(define assign* ;Recursively assigns variables in the state and returns the state
  (lambda (variable expression state parsetree functions main) 
    (cond
      ((and (pair? expression) (eq? (car expression) '=)) (assign* variable (second expression) (assign* (second expression) (third expression) state parsetree functions main) parsetree functions main))
      ((eq? (first_state_variable state) variable)
       (assign_variable variable expression state parsetree functions main))
      
      (else ((lambda (memoization_variable)
               (cons (cons (car (car state)) (car memoization_variable)) (cons (cons (first_state_value state) (cadr memoization_variable)) '()) ) )
             (assign* variable (value* expression state parsetree functions main) (state_cdr state) parsetree functions main)))

      )))
      

(define assign_variable ;Set the variable in the state to the given value
  (lambda (variable expression state parsetree functions main)
    (cons (car state) (cons (cons (value* expression state parsetree functions main) (cdr (cadr state)) ) main))
    ))

(define next_command ;Returns the first variable in a list
  (lambda (list)
    (cond
      ((pair? (car list)) (car (car list)))
      (else (car list)))))
;These are the same and accomplish the same thing, but it helps to understand the code in other areas of this project
(define first_state_variable ;Returns the first variable name in the state binding
  (lambda (state)
    (next_command state)))

(define first_state_value ;Returns the first value in the state binding
  (lambda (state)
    (car (cadr state))))

(define state_cdr ;Returns the state without the first variable assignment (without the first elements of the binding lists)
  (lambda (state)
    (cons (cdr (car state)) (cons (cdr (cadr state)) '()))))
    
(define value* ;Takes a rule and state and produces a value
  (lambda (expression instate parsetree functions main)
    (cond
      ((null? expression)
       (error 'null\ expression))
      ((and (pair? expression) (eq? (car expression) 'funcall)) (state parsetree instate '() functions main (lambda (v) v)))
      ((boolean? expression) expression)
      ((number? expression) expression)
      ((symbol? expression) (cond
                              ((eq? expression 'true) #t)
                              ((eq? expression 'false) #f)
                              (else
                               (value_for_variable expression instate))))
      (else (if (and (pair? expression) (is_state (cdr expression)))
                (car expression)

       (if (third? expression) (operation (consthree (car expression) (value* (second expression) instate (cons (second expression) '()) functions main) (value* (third expression) instate  (cons (third expression) '()) functions main)) instate (cons (third expression) '()) functions)
                (operation (cons (car expression) (cons (value* (second expression) instate parsetree functions main) '())) instate parsetree functions))))
      )))

(define operation ; + - * / % (supports unary -)
  (lambda (expression state parsetree functions)
    (cond
      ((equal? (car expression) '+) (+ (value* (second expression) state (second expression) functions '()) (value* (third expression) state (third expression) functions '())))
      ((equal? (car expression) '-) (if (third? expression)
                                        (- (value* (second expression) state parsetree functions '()) (value* (third expression) state parsetree functions '()))
                                        (- (value* (second expression) state parsetree functions '()))))
      ((equal? (car expression) '*) (* (value* (second expression) state parsetree functions '()) (value* (third expression) state parsetree functions '())))
      ((equal? (car expression) '/) (truncate (/ (value* (second expression) state parsetree functions '()) (value* (third expression) state parsetree functions '()))))
      ((equal? (car expression) '%) (modulo (value* (second expression) state parsetree functions '()) (value* (third expression) state parsetree functions '())))
      (else (boolean expression state))

      )))

(define value_for_variable ;Gets the value for a variable in the given state. Returns the value
  (lambda (variable instate)
    (cond
      ((null? instate) (error 'variable\ not\ declared))
      ((member? variable (top_variables instate)) (value_of_state variable (car instate)))
      (else (value_for_variable variable (cdr instate))))))

(define value_of_state
  (lambda (variable state)
    (cond   
      ((not (member? variable (car state))) (error 'variable\ not\ declared))
      ((eq? (first_state_variable state) variable) (if (null? (first_state_value state)) (error 'variable\ not\ defined) (first_state_value state))) ;If value is not yet defined, throw error. Else, return value.
      (else (value_of_state variable (state_cdr state)))
      )))

(define newstate ;Creates a new state defined by the current state and the condition
  (lambda (condition state)
    (cond
      ((not (pair? condition)) state)
      ((and (not (pair? (second condition))) (not (pair? (third condition)))) state)
      ((and (not (pair? (third condition))) (pair? (second condition)) (eq? (car (second condition)) '=)) (assign* (second (second condition)) (third (second condition)) state '() '() '()))
      ((and (not (pair? (second condition))) (pair? (third condition)) (eq? (car (third condition)) '=)) (assign* (second (third condition)) (third (third condition)) state '() '()'()))
      ((and (pair? (second condition)) (pair? (third condition)) (eq? (car (second condition)) '=) (eq? (car (third condition)) '=))
       (assign* (second (third condition)) (third (third condition)) (assign* (second (second condition)) (third (second condition)) state '() '() '())'()'() '()))
      (else state)
      )))

(define newcondition ;Creates a new condition based on the condition. Useful for nested expressions (replace the computed value with the variable) - used in conjunction with newstate.
  (lambda (condition)
    (cond
      ((not (pair? condition)) condition)
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
      ((and (not (pair? parsetree)) (equal? parsetree 'true)) #t)
      ((and (not (pair? parsetree)) (equal? parsetree 'false)) #f)

      ((equal? (car parsetree) '<) (< (value* (second parsetree) instate '() '() '()) (value*(third parsetree) instate '() '() '())))
      ((equal? (car parsetree) '>) (> (value* (second parsetree) instate '() '() '()) (value*(third parsetree) instate '() '() '())))
      ((equal? (car parsetree) '==) (= (value* (second parsetree) instate '() '() '()) (value*(third parsetree) instate '() '() '())))
      ((equal? (car parsetree) '!=) (not (= (value* (second parsetree) instate '() '() '()) (value*(third parsetree) instate '() '() '()))))
      ((equal? (car parsetree) '<=) (<= (value* (second parsetree) instate '() '() '()) (value*(third parsetree) instate '() '() '())))
      ((equal? (car parsetree) '>=) (>= (value* (second parsetree) instate '() '() '()) (value*(third parsetree) instate '() '() '())))
      ((equal? (car parsetree) '&&) (and (value* (second parsetree) instate '() '() '()) (value* (third parsetree) instate '() '() '())))
      ((equal? (car parsetree) '||) (or (value* (second parsetree) instate '() '() '()) (value* (third parsetree) instate '() '() '())))
      ((equal? (car parsetree) '!) (not (value* (second parsetree) instate '() '() '())))
      )))

(define member? ;Returns #t if list contains atom
  (lambda (atom list)
    (cond
      ((null? list) #f)
      (else (or (eq? (car list) atom) (member? atom (cdr list)))))))


(define second? ;Returns #t iff list contains a second item
  (lambda (list)
    (and (not (null? list)) (not (null? (cdr list))))))

(define third? ;Returns #t iff list contains a third item
  (lambda (list)
    (and (second? list) (not (null?  (cdr (cdr list)))))))

(define fourth? ;Returns #t if list contains a fourth item
  (lambda (list)
    (not (null? (cdr (cdr (cdr list)))))))


(define value? ;Returns #t if list contains a third item
  (lambda (list)
    (not (null?  (cdr (cdr list))))))