#lang racket #| CSC324 Fall 2022: Assignment 4 |#

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide typeof typeo)

;-------------------------------------------------------------------------------
; * Task 1: A Simple Type Inferencer *
;-------------------------------------------------------------------------------

#|
(typeof expr typeenv)
  expr: An expression following the Adder grammar
  typeenv: An association list containing the mapping of identifiers to types

  Returns the type of the expression expr: 'num, 'str, 'bool, 'error
|#
(define (typeof expr typeenv)
  (cond
    ; Constants
    [(number? expr) 'num]
    [(string? expr) 'str]
    [(boolean? expr) 'bool]
    ; TODO

    ; Identifiers
    [(symbol? expr)
     (if (lookup expr typeenv)
         (lookup expr typeenv)
         'error)
     ]
    ; TODO

    ; Builtins
    [(member (first expr) '(+ * - /))
     (if (and (equal? (typeof (cadr expr) typeenv) 'num)
              (equal? (typeof (caddr expr) typeenv) 'num))
         'num
         'error)
     ]
    [(member (first expr) '(> = >=))
     (if (and (equal? (typeof (cadr expr) typeenv) 'num)
              (equal? (typeof (caddr expr) typeenv) 'num))
         'bool
         'error)
     ]
    [(equal? (first expr) '++)
     (if (and (equal? (typeof (cadr expr) typeenv) 'str)
              (equal? (typeof (caddr expr) typeenv) 'str))
         'str
         'error)
     ]
    [(equal? (first expr) '!)
     (if (equal? (typeof (cadr expr) typeenv) 'bool)
         'bool
         'error)
     ]
    [(equal? (first expr) 'num->str)
     (if (equal? (typeof (cadr expr) typeenv) 'num)
         'str
         'error)
     ]
    [(equal? (first expr) 'len)
     (if (equal? (typeof (cadr expr) typeenv) 'str)
         'num
         'error)
     ]
    ; TODO
    ; Function Calls
    [(symbol? (first expr))
     (let* ([fst (typeof (first expr) typeenv)])
       (if (check (first fst) (rest expr) typeenv)
           (cadr fst)
           'error))  
     ]
    ; TODO
    [else 'error]
    ))

; Helper functions for Task 1
#|
(lookup key alst)
  elem: A key in the association list
  alst: An association list 

  Returns the value corresponding to the first time the key appears in the
  association list, or #f if the key does not exist.

  Examples:
  > (lookup 'b '((a . 3) (b . 4)))
  4
  > (lookup 'b '((a . 3) (b . 4) (b . 5)))
  4
  > (lookup 'c '((a . 3) (b . 4) (b . 5)))
  #f
|#
(define (lookup key alst)
  (cond
    [(empty? alst) #f]
    [(equal? key (car (first alst))) (cdr (first alst))]
    [else (lookup key (rest alst))]
    ))

; Add your helper functions here
(define (check flst blst typeenv)
  (cond
    [(empty? flst) #t]
    [else (if (equal? (car flst) (typeof (car blst) typeenv))
              (check (rest flst) (rest blst) typeenv)
              #f)]
    ))
;-------------------------------------------------------------------------------
; * Task 2: A Type Inferencer Relation in miniKanren
;-------------------------------------------------------------------------------

#|
(typeo expr typeenv type)
  expr: An expression following the Adder grammar
  typeenv: An association list containing the mapping of identifiers to types
  type: The type of the expression

  The relational form of the `typeof` function
|#
(define (typeo expr env type)
  (conde
   ; constants: numbero, stringo, and boolo are miniKanren builtin relations
   ; TODO
   ((numbero expr)
    (== type 'num))
   ((stringo expr)
    (== type 'str))
   ((boolo expr)
    (== type 'bool))
 
   ; identifier: symbolo is a miniKanren builtin relation
   ; TODO
   ((symbolo expr) (lookupo expr env type))

   ; builtins
   ; TODO
   ((fresh (fst rst arg1 arg2)
           (== expr (cons fst (list arg1 arg2)))
           (typeo arg1 env 'num)
           (typeo arg2 env 'num)
           (== type 'num)
           (conde ((== fst '+))
                  ((== fst '-))
                  ((== fst '*))
                  ((== fst '/))
                  )))
   ((fresh (fst rst arg1 arg2)
           (== expr (cons fst (list arg1 arg2)))
           (typeo arg1 env 'num)
           (typeo arg2 env 'num)
           (== type 'bool)
           (conde ((== fst '>))
                  ((== fst '=))
                  ((== fst '>=))               
                  )))
   ((fresh (fst rst arg1 arg2)
           (== expr (cons fst (list arg1 arg2)))
           (typeo arg1 env 'str)
           (typeo arg2 env 'str)
           (== type 'str)
           (== fst '++)
           ))
   ((fresh (fst rst arg1)
           (== expr (list fst arg1))
           (typeo arg1 env 'bool)
           (== type 'bool)
           (== fst '!)
           ))
   ((fresh (fst rst arg1)
           (== expr (list fst arg1))
           (typeo arg1 env 'num)
           (== type 'str)
           (== fst 'num->str)
           ))
   ((fresh (fst rst arg1)
           (== expr (list fst arg1))
           (typeo arg1 env 'str)
           (== type 'num)
           (== fst 'len)
           ))
   
   ; function calls
   ;(typeof '(g 3) '((f . ((num) num)) (g . ((num) num))))
   ; TODO (typeo '((lambda (x) x) 3) '() out))
   ((fresh (fep rep feva fst sec)
           (== expr (cons fep rep)) ;fep-->'(lambda (x) x),rep-->'(3) 
           (typeo fep env feva) ;feva--> 
           (== feva (list fst sec)) ;fst-->   ,sec-->
           (checko fst rep env)
           (== type sec)
           ))
   ; function definitions
   ; TODO (run 1 (out) (typeo '((lambda (x) x) 3) '() out)) --> '(num)
   ; base--(run 1 (out) (typeo '(lambda (x) (g x)) '((g . ((num) num))) out)) -->  '( ((num) num)) )
   ;`(lambda (,x) ,body)     '((f . ((num) num)) (g . ((num) str)))
   ((fresh (param body arg rarg evarg newenv tp)
           (conde ((== expr `((lambda (,param) ,body) ,arg))
                   (typeo arg env evarg)    ;evaluate the arg(3 -> 'num)
                   (appendo env (list (cons param evarg)) newenv) ;append the evaluate arguement to env 
                   (typeo body newenv tp)
                   (== tp type))
                  ((== expr `(lambda (,param) ,body))
                   (== body (cons arg rarg))
                   (typeo arg env evarg)
                   ;(appendo env (list (cons param evarg)) newenv) 
                   ;(typeo body newenv tp)
                   (== evarg type)
                   ))))
   ))


; Helper functions for Task 2
#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#
(define (lookupo key alst value)
  (conde ((== alst '())
          (== value #f))
         ((fresh (k v rest)
                 (== alst (cons (cons k v) rest))
                 (conde ((== key k)
                         (== value v))
                        ((=/= key k)
                         (lookupo key rest value)))

                 ))))

#|
(boolo expr)
  Succeeds when expr is a boolean
|#
(define (boolo expr)
  (conde ((== expr #t))
         ((== expr #f))))

; Add your helper functions/relations here
(define (checko flst blst typeenv)
  (conde
    ((== flst '()) (== blst '()))
    ((fresh (arg1 res1 arg2 res2)
            (== flst (cons arg1 res1))
            (== blst (cons arg2 res2))
            (typeo arg2 typeenv arg1)
            (checko res1 res2 typeenv)
            ))))

(define (appendo xs ys xsys)
  (conde ((== xs '())
          (== xsys ys))
         ((fresh (x rxs rxsys)
                 (== xs (cons x rxs))
                 (== xsys (cons x rxsys))
                 (appendo rxs ys rxsys)))))

;(run 1 (out) (typeo '(lambda (x) (g x)) '((g . ((num) num))) out))
;-------------------------------------------------------------------------------
; * Task 3 (Optional): Synthesizing Programs *
;-------------------------------------------------------------------------------

#|
(fill-in lvar expr type n)
  lvar: The logic variable to be filled in
  expr: An expression following the Adder grammar, with a logic variable
        somewhere in the expression to be filled in.
  type: The desired type of the expression after BLANK is filled in.
  n:    The maximum number of results to return.

  Macro that runs a miniKanren query that will replace the symbol `BLANK`
  in the Adder expression `expr` so that the type of the expression
  is consistent with `type`. The query returns at most `n` results.
|#

(define-syntax fill-in
  (syntax-rules ()
    [(fill-in lvar expr type n)
     (void) ; TODO
     ]))


