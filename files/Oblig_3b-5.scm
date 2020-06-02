;; På gruppe: nasiraa, kajai and knutsi

(load "evaluator.scm")


;;;1a
;; (foo 2 square) returnerer 0.
;; foo har blitt lagt til i listen av variabler og prosedyrekroppen har blitt
;; lagt til i listen med bindinger.
;;
;; Når foo kalles blir uttrykkene i prosedyrekroppen evaluert i en ny
;; omgivelse med de aktuelle parameter-bindingene. Den første cond'en står på
;; posisjonen til en operator/prosedyre, og blir evaluert som en special form.
;; Uttrykket blir evaluert i henhold til evalueringsreglene for cond.
;; (= cond 2), her er cond en variabel som er knyttet til prosedyrens parameter.
;; I søk etter verdien til cond søkes det først i rammen/omgivelsen vi
;; befinner oss i. Her er cond knyttet til verdien 2. (= 2 2) er sant,
;; og 0 blir returnert. 
;;
;;
;;
;; (foo 4 square) returnerer 16.
;;
;; Se over ang. cond. Her er (= cond 2) usant, fordi cond er knyttet til 4.
;; Den første else'en står på posisjonen til en operator/prosedyre,
;; og blir evaluert som en special form. (else cond), her er else og cond
;; variabler knyttet til prosedyrens parametere. else er knyttet til square
;; square er ikke knyttet til noe i denne omgivelsen, derfor fortsettes søket
;; til omgivelsen rundt. Her er square knyttet til en prosedyre. Denne
;; prosedyren blir kalt med cond som parameter, hvor cond er knyttet til verdien
;; 4. 
;;
;;
;;
;;(cond ((= cond 2) 0)
;;      (else (else 4)))
;; returnerer 2
;;
;; Den første cond'en står på posisjonen til en operator/prosedyre, og blir
;; evaluert som en special form. (= cond 2), her er cond en variabel som er
;; bundet til en verdi i den globale omgivelsen, fordi det er her vi befinner
;; oss. Verdien til cond er 3, og det er derfor usant. Den første else'en står
;; på posisjonen til en operator/prosedyre, og blir evaluert som en special
;; form. Deretter kalles else, dette er en variabel som er knyttet til en
;; prosedyre. else er lagt til i variabellisten i den globale omgivelsen, og
;; else sin prosedyrekropp er lagt til i listen for bindinger. else-prosedyren
;; deler et tall på 2, så når 4 sendes inn som parameter blir resultatet 2.



;2a
;(set! the-global-environment (setup-environment))
;(define global the-global-environment)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        (list '1+
              (lambda (x) (+ x 1)))
        (list '1- 
              (lambda (x) (- x 1)))      
;;      her kan vi legge til flere primitiver.
        ))


(set! the-global-environment (setup-environment))
;(read-eval-print-loop)



;;; 2b


(define global the-global-environment)

(define (install-primitive! name proc)
  (let ((primitive-proc (list 'primitive proc)))
    (define-variable! name primitive-proc global)))


(install-primitive! 'square (lambda (x) (* x x)))

;global
;(read-eval-print-loop)

;; 3 a

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t)
        ((or? exp) #t)
        (else #f)))


(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (display (cdr exp)) (newline)(eval-and (cdr exp) env))
        ((or? exp) (display (cdr exp)) (newline)(eval-or (cdr exp) env))))


(define (eval-and exp env)
  (cond ((last-exp? exp) (mc-eval(car exp) env))
        ((false? (mc-eval (car exp) env)) #f)
        (else (eval-and (cdr exp) env))))


(define (eval-or exp env)
  (cond ((last-exp? exp) (mc-eval(car exp) env))
        ((true? (mc-eval (car exp) env)) #t)
        (else (eval-or (cdr exp) env))))




;(read-eval-print-loop)


;; Oppgave 3b
(define (else? exp) (tagged-list? exp 'else))
(define (then? exp) (tagged-list? exp 'then))


(define (eval-if exp env)
  (if (then? (cddr exp))
      (eval-elsif exp env)
      (if (true? (mc-eval (if-predicate exp) env))
         (mc-eval (if-consequent exp) env)
         (mc-eval (if-alternative exp) env))))

(define (eval-elsif exp env)
   (if (not(else? exp))
       (if (true? (mc-eval (if-predicate exp) env))
           (mc-eval (if-consequent (cdr exp)) env)
           (eval-elsif (cddddr exp) env))
       (mc-eval (cadr exp) env)))
           
           ;(mc-eval (if-alternative exp) env)
           ;(eval-elsif (cddddr exp) env))))


(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (if (else? (cdddr exp))
         (car (cddddr exp)) 
         (cadddr exp))
      'false))


(set! the-global-environment (setup-environment))
(define global the-global-environment)
;(read-eval-print-loop)

(define exp1 '(if (= 2 3)
                  then 'hei
               elsif (= 2 2)
                  then 'nei
               elsif (= 0 2)
                  then 'nei
               else '(dette er else)))

(define exp2 '(if (= 4 4)
                  1
                  2))

;(then? (cddr exp1))
;;(car (cddddr(cddddr exp1)))
(mc-eval exp1 global)
                