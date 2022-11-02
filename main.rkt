#lang racket
(require "utility.rkt")
(require "runner.rkt")
(require "parser.rkt")


;(define scope '((a 1) (b 2) (c 5)))


(define env '((global (a 1) (b 2) (c 5))))

;(define sample-code '(call (function ()(ask (bool != a b) (math - a b) (math + a b))) (a)))
(define sample-code '(call (function (a) (call (function (r) a ) (a))) (5))) 
;(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)

;(define parsed-neo-code (neo-parser '(call (function (a) (local-vars ((x 3) (o 10) (w 12)) (call (function (b)(math + a (math * b x)))(2)) ))(3)) )) ;multiplying element a with element b and adding element x
;(define parsed-neo-code (neo-parser '(call (function (a) (call (function (r) a ) (a))) (5)))) ;//inner call expression should not know the variable a's value
;(run-neo-parsed-code parsed-neo-code env)


