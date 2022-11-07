#lang racket
(require "utility.rkt")

; resolve a value from variable enviroment
(define resolve_scope
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) varname))
      )
    )
  )

(define resolve_env
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((null? (car environment)) (resolve_env (cdr environment) varname))
      ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname))
      (else (let ((resolved_result (resolve_scope (car environment) varname)))
              (if (equal? resolved_result #false)
                  (resolve_env (cdr environment) varname)
                  resolved_result
                  )
              )
            )
    )
  )
  )

                           

(define extended-scope
  (lambda (list-of-varname list-of-value scope)
    (cond
      ((null? list-of-varname) scope)
      ((null? list-of-value) scope)
      (else (extended-scope (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             scope)))
      )
    )
  )

(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    (let ((new_scope (extended-scope list-of-varname list-of-value'()))
          (pop_off_env (pop_env_to_global_scope env)))
      (cons new_scope pop_off_env)
    )
  )
 )

(define pop_env_to_global_scope 
  (lambda (env)
    (cond
      ((null? env) #false)
      ((equal? (length env) 1)
       (if (equal? (car (car env)) 'global) env
           #false))
      (else (pop_env_to_global_scope (cdr env)))
      )
    )
  )

(define extend_local_scope
  (lambda (list-of-varname list-of-value env)
    (cond
      ((null? env) #false)
      ;check the first scope is local scope or not
      ((equal? (caar env) 'global) (push_scope_to_env list-of-varname list-of-value env))
      ;use extend_scope function to add new variables into the local scope
      (else (cons (extended-scope list-of-varname list-of-value (car env))
                  (pop_env_to_global_scope env)))
     )
    )
  )
                   


;(app-exp (func-exp (params x) (body-exp (var-exp x))) (var-exp a))
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve_env env (cadr parsed-code)))
       ;(bool-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code parsed-code env))
      ;(math-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      ((equal? (car parsed-code) 'let-exp)
       
       (run-let-exp parsed-code env))
      (else (run-neo-parsed-code
             (cadr parsed-code)
             (push_scope_to_env (cadr (cadr (cadr parsed-code)))
                                 (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
                                  env)
             )
            )
      )
    )
  )

; run bool parsed code
(define run-bool-parsed-code
  (lambda(parsed-code env)
    (let ((op (elementAt parsed-code 0))
           (num1 (run-neo-parsed-code (elementAt parsed-code 1) env))
           (num2 (run-neo-parsed-code (elementAt parsed-code 2) env)))
           (cond
             ((equal? op '>) (> num1 num2))
             ((equal? op '<) (< num1 num2))
             ((equal? op '>=) (>= num1 num2))
             ((equal? op '<=) (<= num1 num2))
             ((equal? op '==) (= num1 num2))
             ((equal? op '!=) (not (= num1 num2)))
             ((equal? op '&&) (and num1 num2))
             ((equal? op '||) (or num1 num2))
             (else (not num1))
        )
      )
    )
  )

(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2)) ;+ 1 1)
      ((equal? op '-) (- num1 num2))  ;- 1 1
      ((equal? op '*) (* num1 num2)) ;* 1 1
      ((equal? op '/) (/ num1 num2))  ;/ 1 1 float division
      ((equal? op '//) (quotient num1 num2))  ;// 1 1 interger division
      ((equal? op '%) (modulo num1 num2))  ;% 1 1 modulo
      (else #false)
      )
     )
   )


 (define run-let-exp
   (lambda (parsed-code env)
     (let* ((list-of-names (getVarnames (elementAt parsed-code 1)))
           (list-of-values (getValues (elementAt parsed-code 1)))
           (new_env (extended-scope list-of-names list-of-values env))
           (body (elementAt parsed-code 2)))
     (run-neo-parsed-code body new_env)
     )
   )
   )

(provide (all-defined-out))