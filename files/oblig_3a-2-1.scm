;; På gruppe: nasiraa, kajai and knutsi
(load "prekode3a.scm")

;; Oppgave 1 a og b
(define (umem msg proc tabl1 tabl2)
  (let ((pros (lambda args
                (or (lookup args tabl2)
                    (let ((result (apply proc args)))
                      (insert! args result tabl2)
                      result)))))
    (insert! pros proc tabl1)pros))
  

(define mem                      ;tabell for å huske memorisert prosedyrer
  (let ((proc-table (make-table)) (t (make-table)))
    (lambda (msg proc)    
      (if(eq? msg 'unmemoize)
         (lookup proc t)
         (let((table (make-table)))
           (insert! proc 
                    (lambda args
                      (let ((ans (lookup args table)))
                        (cond (ans)
                              (else (let ((ret (apply proc args)))
                                      (insert! args ret table)
                                      ret)))))  proc-table)
           ;Lagrer verdier for bruke dem senere med hjelpeprosedyre
           (lookup proc proc-table) (umem msg proc t table))))))


(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)
(set! fib (mem 'unmemoize fib))
(fib 3)

;; 1 c
;; I oppgave 1a brukes set! til å sette fib til å være prosedyren som mem
;; returnerer. Denne prosedyren leter etter svaret til det tallet som sendes inn
;; i en tabell, dersom den ikke finner svare kaller den på den opprinnelige fib
;; og legger inn alle resultatene den finner på veien. I og med at fib kaller
;; på fib rekursivt vil denne kalle på den nye fib og dermed blir verdiene lagt
;; inn i tabellen for alle delresultatene.

;; I oppgave 1c kalles mem-prosedyren på nytt hver gang mem-fib kalles. I og med
;; at fib ikke er satt til å være den memioserte versjonen, men til originale
;; fib vil den kun lagre resultatet til det første kallet, deretter vil fib
;; håndtere resten. På den måten returnere (mem-fib 3) 2 andre gangen den blir
;; kalt, men dersom man kaller (mem-fib 2) etter dette vil den rekursivt beregne
;; resultatet på nytt. 

;(define mem-fib (mem 'memioze fib))
;(mem-fib 3)
;(mem-fib 3)
;(mem-fib 2)
;(mem-fib 2)
;(mem-fib 3)


;; Oppgave 2 a

(define (list-to-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (list-to-stream (cdr lst)))))
 

    (define (stream-to-list stream . n)
  (if (stream-null? stream)
      the-empty-stream
      (if (null? n)
          (cons (stream-car stream) (stream-to-list (stream-cdr stream)))
          (if (= (car n) 0)
              '()
              (cons (stream-car stream)
                   (stream-to-list (stream-cdr stream) (- (car n) 1)))))))


;2b
(define (stream-map proc . argstreams)
     (if (stream-null? (car argstreams))
         the-empty-stream
         (cons-stream
           (apply proc  (map  stream-car argstreams))
           (apply stream-map
                  (cons proc (map stream-cdr argstreams))))))



(stream-map + (list-to-stream '(1 4 ))
            (list-to-stream '(2 4))(list-to-stream '(2 4)))

;;2c
;; Siden streams kan være uendelige lister så kan man ikke teste for duplikater.
;; Man vil aldri bli ferdig med å lete igjennom hele listen, i og med at den er
;; uendelig, og derfor vil heller aldri sammenligningen med første element og
;; resten av listen fullføre. 

;2d


(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter
                     (lambda (x)
                       (not(eq? (stream-car stream) x)))
                     (stream-cdr stream))))))

(show-stream(remove-duplicates(list-to-stream '(1 2 1 2  3 3 3 4 3 ))) 4)


;2e
(define x
       (stream-map show (stream-interval 0 10)))

;; (stream-ref x 5) -> (hva show printer 0 1 2 3 4 5) (hva stream-refprinter 5)
;; (stream-ref x 7) -> (hva show printer 0 1 2 3 4 5 6 7)
;; (hva stream-ref printer 7)
;; Stream-map tar in show og stream-intervalet fra 0 til 10. stream-ref
;; refererer til de n'te elementet i intervalet i stream-interval
;; som er fra 0-10. Så show vill printe fra 0 til og med det tallet du sender
;; inn. Siste print statment kommer fra stream-ref som er 5 og 7 


; 2f

(define (mul-streams . argstreams)
   (if (stream-null? (car argstreams))
         the-empty-stream
 (apply stream-map * argstreams)))


(mul-streams (list-to-stream '(2 4 ))(list-to-stream '(4 4))
             (list-to-stream '(5 4)))
(mul-streams '()(list-to-stream '(2 4 )))



;;; 2 g

(define factorials (cons-stream 1 (mul-streams nats factorials )))

(stream-ref factorials 5)

