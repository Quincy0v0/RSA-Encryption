#lang racket

(define (cvt s)
  (define (encipher s)
        (cond
          [(char-alphabetic? s) (- (char->integer s) 64)]
          [(char-numeric? s) (char->integer s)]
          [else 0]))
     (map (lambda (i)
         (encipher i))
             (string->list (string-upcase s))))
;the encipher function
(define (cvtinv s)
  (define (decipher s)
        (cond
          [(= s 0) #\space]
          [(< s 27) (integer->char (+ 64 s))]
          [(char-numeric? (integer->char s))(integer->char s)]
          [else #\space]
          ))
     (list->string
      (map (lambda (i)
         (decipher i))
           s)))
;the decipher function



;The following is a stupid, inefficient mod function
;(define (mod b n k)
;;   (define (pow b n)
;        (cond
;             [(= n 1) b];base case- n=1
;             [(= (modulo n 2) 0) (* (pow b (/ n 2)) (pow b (/ n 2)))];n is even
;             [(= (modulo n 2) 1) (* (pow b (/ (- n 1) 2)) (* (pow b (/ (- n 1) 2)) b))];n is odd
;             ))
;     (modulo (pow b n) k))

;This is a good mod function
(define (mod b n k)
        (cond
             [(= n 1) (modulo b k)];base case: n=1
             [(= (modulo n 2) 0) (modulo (* (mod b (/ n 2) k) (mod b (/ n 2) k)) k)];n is even
             [(= (modulo n 2) 1) (modulo (* (mod b (/ (- n 1) 2) k) (* (mod b (/ (- n 1) 2) k) b)) k)];n is odd
             ))

;;Problem 4- There is no problem3 in the PDF
(define (encode s N e)
  (writeln (let ([tp (cvt s)])tp))
  (map (lambda (i)
         (mod i e N))
           (cvt s)))
;"RSA encode" a given string
;N=209, e=7, d=103

(define (decode s N d)
  (writeln (let ([tp (map (lambda (i)
         (mod i d N))
            s)])tp))
   (cvtinv
    (map (lambda (i)
         (mod i d N))
            s)))
;decodes a given encoded list of numbers
;(decode '(186 203 46 174 179 128 1 94 82 168 168 174) 209 103)
;"JOHN BARDEEN"

;;Problem 5&6
(define (nvl lst)
  (cond [(= (modulo (length lst) 3) 0) lst]
        [(= (modulo (length lst) 3) 2) (append lst '(0))]
        [(= (modulo (length lst) 3) 1) (append lst '(0 0))]))
;normalize the list i.e. add zeros to the end if not divisible by 3

(define (2d->3d lst)(list
  (+ (* 10 (list-ref lst 0)) (floor (/ (list-ref lst 1) 10)))
  (+ (* (modulo (list-ref lst 1) 10) 100) (list-ref lst 2))))
;;converts the first 3 elements of the 2-digit list into 2* 3-digit elements;

(define (2->3all ls)
(define(pow part lst)
    (cond
      [(<= (length part) 3) (append (2d->3d part) lst)]
      [else (append  (2d->3d part) (pow (list-tail part 3) lst))]))
  (pow (nvl ls) empty))
;;recursively calls "2d->3d" and converts the whole list to a 3-digit one   

(define (3d->2d lst)(list
  (floor (/ (list-ref lst 0) 10))
  (+ (* (modulo (list-ref lst 0) 10) 10) (floor (/ (list-ref lst 1) 100)))
  (modulo (list-ref lst 1) 100)))
;;converts the first 2 elements of the 3-digit list into 3* 2-digit elements

(define (3->2all ls)
(define(pow part lst)
    (cond
      [(<= (length part) 2) (append (3d->2d part) lst)]
      [else (append  (3d->2d part) (pow (list-tail part 2) lst))]))
  (pow (nvl ls) empty))
;;recursively calls "3d->2d" and converts the whole list to a 2-digit one   

(define (encode4d s N e)
  (writeln (let ([tp (cvt s)])tp))
  (map (lambda (i)
         (mod i e N))
           (cvt (2->3all s))))
;;Does the same thing as "encode" except converting the 2-digit list into a 3-digit list
(define (decode4d s N d)
  (writeln (let ([tp (map (lambda (i)
         (mod i d N))
            s)])tp))
   (cvtinv
    (3->2all
     (map (lambda (i)
         (mod i d N))
             s))))
;;Does the same thing as "decode" except converting the 3-digit list into a 2-digit list