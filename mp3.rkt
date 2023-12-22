#lang racket

(provide parse
         desugar
         eval
         load-defs
         repl)

;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (fn arg) #:transparent)

;; boolean
(struct boolean-exp (bool) #:transparent)

;; if
(struct if-exp (boolean-exp true false) #:transparent)

;; relational exp
(struct relational-exp (op lhs rhs) #:transparent)

;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; arithmetic expression
    [(list (and op (or '+ '*)) lhs rhs)
     (arith-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; relational expression
    [(list (and op (or '= '<)) lhs rhs)
     (relational-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; if expression
    [(list 'if boolean-exp T-exp F-exp)
     (if-exp (parse boolean-exp) (parse T-exp) (parse F-exp))]

    ;; identifier (variable)
    [(? symbol?)
     (var-exp sexp)]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]

    ;; lambda expression -- modified for > 1 params
    [(list 'lambda (list ids ...) body)
     (lambda-exp ids (parse body))]

    ;; function application -- modified for > 1 args
    [(list f args ...)
     (app-exp (parse f) (map parse args))]

    ;; for boolean expression
    [(? boolean?)
     (boolean-exp sexp)]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Desugar-er -- i.e., syntax transformer
(define (desugar exp)
  (match exp
    ((arith-exp op lhs rhs)
     (arith-exp op (desugar lhs) (desugar rhs)))

    ((let-exp ids vals body)
     (let-exp ids (map desugar vals) (desugar body)))
    
    ((lambda-exp ids body)
     (foldr (lambda (id lexp) (lambda-exp id lexp))
            (desugar body)
            ids))
    
    [(if-exp boolean-exp T-exp F-exp)
     (if-exp (desugar boolean-exp) (desugar T-exp) (desugar F-exp))]
    
    ((app-exp (var-exp '-) (list lhs rhs))
     (arith-exp "+" (desugar lhs) (arith-exp "*" (int-exp -1) (desugar rhs))))

    ((app-exp (var-exp 'and) (list bexps ...))
     (foldr (lambda (exp strt) (if-exp exp strt (boolean-exp #f)))
            (boolean-exp #t)
            bexps))
    
    ((app-exp (var-exp 'or) (list bexps ...))
     (foldr (lambda (exp strt) (if-exp exp (boolean-exp #t) strt))
            (boolean-exp #f)
            bexps))

    ((app-exp (var-exp '>) (list lhs rhs))
     (relational-exp "<" rhs lhs))

    ((app-exp (var-exp '<=)  (list lhs rhs))
     (desugar
      (app-exp
       (var-exp 'or)
       (list
        (relational-exp "<" lhs rhs)
        (relational-exp "=" lhs rhs)))))
     

    ((app-exp (var-exp '>=) (list lhs rhs))
     (desugar
      (app-exp
       (var-exp 'or)
       (list
        (relational-exp "<" rhs lhs)
        (relational-exp "=" lhs rhs)))))
    
    ((app-exp (var-exp 'cond) (list (app-exp bool-exp (list R-exp)) ... (app-exp (var-exp 'else) (list rest))))
     (foldr (lambda (exp exp2 strt) (if-exp exp exp2 strt))
            rest
            bool-exp
            R-exp))
    
    ((app-exp f args)
     (foldl (lambda (id F-exp) (app-exp F-exp id))
            (desugar f)
            (map desugar args)))
    
    (_ exp)))

;; Interpreter
(define (eval expr [env '()])
  (match expr
    ;; int literal
    [(int-exp val) val]

    ;; arithmetic expression
    [(arith-exp "+" lhs rhs)
     (+ (eval lhs env) (eval rhs env))]
    [(arith-exp "*" lhs rhs)
     (* (eval lhs env) (eval rhs env))]

    ;; relational expression
    [(relational-exp "=" lhs rhs)
     (eq? (eval lhs env) (eval rhs env))]
    [(relational-exp "<" lhs rhs)
     (< (eval lhs env) (eval rhs env))]
     
    ;; variable binding
    [(var-exp id)
     (let ([pair (assoc id env)])
       (if pair (cdr pair) (error (format "~a not bound!" id))))]

    ;; let expression
    [(let-exp (list (var-exp id) ...) (list val ...) body)
     (let ([vars (map cons id
                      (map (lambda (v) (eval v env)) val))])
       (eval body (append vars env)))]

    ;; lambda expression
    [(lambda-exp id body)
     (fun-val id body env)]

    ;; function application
    [(app-exp f arg)
     (match-let ([(fun-val id body clenv) (eval f env)]
                 [arg-val (eval arg env)])
       (eval body (cons (cons id arg-val) clenv)))]

    ;; boolean
    [(boolean-exp bool) bool]

    ;; if expression
    [(if-exp bool-exp T-exp F-exp)
     (if (eval bool-exp env) (eval T-exp env) (eval F-exp env))]

    ;; basic error handling
    [_ (print expr)]))

;; function value + closure
(struct fun-val (id body env) #:prefab)

;; load definitions (returning env)
(define (load-defs filename)
  (let* ([sexps (file->list filename)]
         [ph (make-placeholder '())]
         [fn-names (map (compose first second) sexps)]
         [fn-vals (map (lambda (foo) (fun-val (second (second foo)) (desugar (parse (third foo))) ph)) sexps)]
         [env (map cons fn-names fn-vals)])
    [placeholder-set! ph env]
    [make-reader-graph env]))


;; REPL
(define (repl [filename #f])
  (let loop ([env (if filename (load-defs filename) '())])
    (let ([stx (desugar (parse (read)))])
      (when stx
        (println (eval stx env))
        (loop env)))))