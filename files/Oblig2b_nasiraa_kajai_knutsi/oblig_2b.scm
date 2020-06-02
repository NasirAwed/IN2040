;;; På gruppe: nasiraa, kajai and knutsi
;;;1 a


(define count 42)

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ 1 count))
      count)))

(define c1 (make-counter))
(define c2 (make-counter))
(c1)
(c1)
(c1)
count
(c2)


;;; 1 b
;; Se vedlagt tegning


;;; 2 a

(define (make-stack items)

  (define (pop!)
    (if (not(null? items))
        (set! items (cdr items))))
  
  (define (push! new_item)
    (set! items (append (reverse new_item) items)))

  (define (dispatch msg . args)
    (cond ((eq? msg 'pop!) (pop!))
          ((eq?  msg 'stack) items)
          ((eq? msg 'push!) (push! args))))
  dispatch)



(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)
(s2 'pop!) 
(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

;;; 2 b
(define (pop! my_stack)
  (my_stack 'pop!))

(define (stack my_stack)
  (my_stack 'stack))

(define (push! my_stack . args)
  (for-each (lambda (x) (my_stack 'push! x)) args))

;; Alternativt
; (define (push-h my_stack items)
;    (if(null? items)
;       '()
;       (my_stack 'push! (car items)))
;    (if(not(eq? items '()))
;     (push-h my_stack (cdr items)))) 
  
;(define (push! . args)
;  (push-h (car args) (cdr args)))

(pop! s1)
(stack s1)
(push! s1 'foo 'fah)
(stack s1)


;;; 3 a
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
(list-ref bar 0)
(list-ref bar 3)
(list-ref bar 4)
(list-ref bar 5)

;;; 3 b
;se vedlagt tegning

;;; 3c


(define (cycle? liste)
  (define (cycle-h listee sett)
    (if (null? listee) 
        #f
        (if (memq listee sett)
            #t
            (cycle-h (cdr listee) (cons listee sett)))))
  (cycle-h liste '()))

;; Cycle har lineært minnebruk


;;; 3 d
;; Ekte lister består enten av den tomme listen eller av par hvor
;; cdr-verdien må bestå av en liste. En liste kan derfor bestå av flere par,
;; men denne kjeden av par må avsluttes med den tomme listen.
;; Dette betyr at en listes aller siste element må være den tomme listen.
;; En sirkulær liste har ikke noe siste element, og dermed ikke den tomme
;; listen som et siste element. 


(cycle? bar)
(cycle? '(hey ho))
(cycle? '(la la la))

;;; 3e

(define (copy-list args)
  (define (iter list)
    (if (null? list)
        '()
        (cons (car list)
              (iter (cdr list)))))
  (iter args))

(define (make-ring liste)
  (let ((org-liste (copy-list liste)))
    (define original liste)
    (set-cdr! liste liste)
    liste))

(define r1 (make-ring '(1 2 3 4)))
r1

;;; 3f
;;Insert og delete kan gøres med konstant tid. Man kan ha en peker som alltid 
;;peker på top elementet (kan være siste element i en liste som peker på det 
;;første) da kan man sette inn og slette med konstant tid. Hvis rotasjon skal 
;;gjøres med konstant tid, så kan man implementere en dobbelt lenket liste.
;;Da vill alle operasjoner kunne bli gjort med konstant tid O(1).
