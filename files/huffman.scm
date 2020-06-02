;;;;
;;;; Prekode til innlevering 2a i IN2040 (H19): Prosedyrer for å jobbe med
;;;; Huffman-trær, fra SICP, Seksjon 2.3.4.
;;;;

;;; Merk at koden under gjør bruk av diverse innebygde kortformer for
;;; kjeder av car og cdr. F.eks er (cadr x) det samme som (car (cdr x)), 
;;; og (caadr x) tilsvarer (car (car (cdr x))), osv. 



;;;
;;; Abstraksjonsbarriere:
;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;;
;;; Dekoding:
;;;

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (if (= bit 0) 
      (left-branch branch)
      (right-branch branch)))



;;;
;;; Sortering av node-lister:
;;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


      

;;;
;;; Diverse test-data:
;;;
(define (decode1 bitss tree)
  (define (decode-10 bits current-branch m)
    (if (null? bits)
        m
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
                    (decode-10 (cdr bits) tree (cons (symbol-leaf next-branch) m))
              (decode-10 (cdr bits) next-branch m)))))
  (decode-10 bitss tree '()))


(define sample-tree
  (make-code-tree 
   (make-leaf 'ninjas 8) 
   (make-code-tree 
    (make-leaf 'fight 5) 
    (make-code-tree 
     (make-leaf 'night 1) 
     (make-leaf 'by 1)))))

;;2G
(define (huffman-leaves n)
  (if (leaf? n)
    (list (symbol-leaf n) (weight-leaf n)
           )
    (list (huffman-leaves (left-branch n))
           (huffman-leaves (right-branch n))
           )
    ))

(huffman-leaves sample-tree)

  
(define sample-code '(0 1 0 0 1 1 1 1 1 0))
;(define sample-code '( 0  ))
;(decode sample-code sample-tree)



(define (expected-codeword-length tree)
  (define (subtree-sum subtree depth)
    (display subtree) (newline)
    (if (leaf? subtree)
      (+ (weight subtree) depth) ;; frekvens * kodelengde
      (+ (subtree-sum (left-branch subtree) (+ depth 1))
         (subtree-sum (right-branch subtree) (+ depth 1)))))
  (/ (subtree-sum tree 0) (weight tree)) )
(expected-codeword-length sample-tree)
;(define (encode message tree)
 ; (if (null? message)
  ;    m
   ;   (lambda (m)(m (encode1 (car message) tree) (encode (cdr message) tree)))
    ;))

  ;(define (encode1 m tree)
  ;(if (null? m)
   ; '()
    ;(lambda(left bit)
     ; (left(left-branc tree))
      ;(bit (if eq? m left) 0 1)
     ;(display m)
   ;(cons bit (encode1 m (choose-branch bit tree))))

    ;))
  
    
    
 ;(decode(encode '(ninjas fight ninjas) sample-tree) sample-tree)

;; Encoding:

;; Fra boken

(define (encode message tree)
  ;(display message) (newline) (newline)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol this-tree)
 ; (display symbol) (newline)
  
  (cond ((null? this-tree) '())
        ((leaf? (left-branch this-tree))
         (cond ((equal? symbol (symbol-leaf (left-branch this-tree)))
             (list 0))
              ((equal? symbol (symbol-leaf (right-branch this-tree)))
               (list 1))
             (else (cons 1 (encode-symbol symbol (right-branch this-tree))))))
        (else (cons 1 (encode-symbol symbol (right-branch this-tree))))))





