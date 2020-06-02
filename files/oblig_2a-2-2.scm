;;; På gruppe: nasiraa, kajai and knutsi
;;;

(load "huffman.scm")

;;; Oppgave 1
;;; a

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))


(p-cons "foo" "bar")
(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))


;;; b

(define foo 42)

;; evalueres til different
((lambda (foo x)
   (if (= foo x)
       'same
       'different)
   ) 5 foo)


;; evaluerer til (towel (42 towel))
((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel)

                 
;;; c

(define (infix-eval items)
  ((lambda (op1 opr op2)
    (opr op1 op2))
    (car items) (cadr items) (caddr items)))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo)
(infix-eval baz)
(infix-eval bar)


;;; d

;(define bah '(84 / 2))
;(infix-eval bah)

;; Resultatet av følgende kall blir en feilmelding.
;; Utfallet blir annerledes fordi det brukes quote i stedet for list.
;; I slike tilfeller blir uttrykket tolket bokstavlig slik at / ikke tolkes
;; som en prosedyre, men som symbolet /. 


;;; Oppgave 2
;;; a

;; Decode leser en sekvens med bits som skal dekodes til en melding som kan
;; bestå av flere ord. For å gjøre dette trengs det en intern hjelpemetode
;; hvor man kan søke nedover i subtrær (ved å sende inn subtrær), og en ytre
;; metode som holder på hele treet. Når første ordet er funnet, må man starte
;; på toppen av treet igjen for å tolke neste del av bitstrengen.
;; Dersom man hadde sendt inn subtreet, uten å beholde det opprinnelige treet
;; ville det ikke vært mulig å finne resten av meldingen etter at første ordet
;; var dekodet. 


;;; b

(define (decode1 bits tree)
  (define (decode-1 bits current-branch m)
    (if (null? bits)
        m
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) m))
              (decode-1 (cdr bits) next-branch m)))))
  (decode-1 bits tree '()))




;;; c

;; Hvis man kaller på den halerekursive versjonen av decode får man meldingen:
;; (night by ninjas fight ninjas). Kaller man på den opprinnelige versjonen får
;; man meldingen: (ninjas fight ninjas by night).

(decode sample-code sample-tree)
(decode1 sample-code sample-tree)

;;; d

;; Føste metoden er hentet fra boken (section 2.3.4, exercise 2.68)
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (encode-symbol symbol this-tree)
  (if (null? this-tree) '()
      (let* ((left (left-branch this-tree))
             (right (right-branch this-tree)))
        (cond
          ((and (equal? symbol (symbol-leaf left))(leaf? left))
           (list 0))
          ((and (equal? symbol (symbol-leaf right))(leaf? right))
           (list 1))
          ((member symbol (symbols left))
           (cons 0 (encode-symbol symbol left)))
          ((member symbol (symbols right))
           (cons 1 (encode-symbol symbol right)))))))

(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)

;;; e

(define (make-code-tree-new n)
  (if(null? (cdr n))
     (car n)
     (let*((l (car n))
           (r (cadr n))
           (new_node (make-code-tree l r)))
       (make-code-tree-new (adjoin-set new_node (cddr n))))))


(define (grow-huffman-tree par)
  (if(null? par)
     '()   
     (make-code-tree-new (make-leaf-set par))))


(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)


;;; f

;; Det brukes 43 bits på å kode meldingen
;; Den gjennomsnittlige lengden på hvert kodeord er ca 2.5 (43 bits / 17 ord)
;; Det minste antall bits man ville trengt for å kode meldingen med en kode med
;; fast lengde over det samme alfabetet er 68 bits.

;; Utregning:
;; log2 16 = 4 -- log2 av antall bokstaver/symboler i alfabetet gir antall bits
;;                hver bokstav må ha for å få unike koder
;; 4 * 17 = 68 -- Antall bits per symbol multiplisert med antall symbol i
;;                meldingen


;(define freqs2 '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3)
;                                (in 2) (ambush 2) (defeat 1) (the 5) (sword 4)
;                                (by 12) (assassin 1) (river 2) (forest 1)
;                                (wait 1) (poison 1)))
;(define codebook1 (grow-huffman-tree freqs2))

;(define msg '(ninjas fight ninjas fight ninjas ninjas fight samurais samurais
;                     fight samurais fight ninjas ninjas fight by night))
;(encode msg codebook1)
;(decode (encode msg codebook1) codebook1)


;;; g

(define (huffman-leaves n)
  (if (leaf? n)
    (list (list (symbol-leaf n) (weight-leaf n))
          )
           
    (append (huffman-leaves (left-branch n))
           (huffman-leaves (right-branch n)))
    ))


(huffman-leaves sample-tree)