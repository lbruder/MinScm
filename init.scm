; vim:lisp:et

; init.scm version 2013-02-19
; A minimal Scheme library
; This is an effort to create a small library of Scheme procedures
; as defined in R4RS (currently) with parts of SRFI-1.
; It can be used in minimal Scheme implementations for quick and easy porting
; or just to toy around and experiment with the language.
; Focus on size and readability, not on performance: If you need speed, use
; this library to get a basic system working, then replace the functions
; one by one with optimized ones (e.g. in C or Assembly).
; Copyright (c) 2013, Leif Bruder <leifbruder@gmail.com>
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted, provided that the above
; copyright notice and this permission notice appear in all copies.
; 
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

; ----------------------------------------------------------------------------
; CONFIGURATION
; ----------------------------------------------------------------------------

(define *sys:epsilon* 0.000001) ; Precision for inexact arithmetic functions

; ----------------------------------------------------------------------------
; RUNTIME REQUIREMENTS
; ----------------------------------------------------------------------------

; Expected special forms:

; if
; define
; set!
; quote
; lambda
; begin

; Expected macro functionality:

; (defmacro name args (form) (form) (form))
; (defmacro name (arg arg arg) (form) (form) (form))
; (defmacro name (arg arg . rest) (form) (form) (form))

; Expected procedures:

; One parameter:
car cdr pair? null? string? boolean? symbol? integer? real? procedure?
vector? char? char->integer integer->char string-length
string->symbol symbol->string vector-length make-string make-vector
sys:display-string ; takes a string to output
sys:exit ; ends the program with the fixnum given as the program's return code

; Two parameters:
cons set-car! set-cdr! + - * / < = quotient remainder eq? apply string-ref
vector-ref

; Three parameters:
string-set! vector-set!

; Temporary requirements until this library is "done":
sys:strtonum ; like string->number, 2 parameters, returns 'nan on error
sys:numtostr ; like number->string, 2 parameters

; ----------------------------------------------------------------------------
; MISSING STUFF
; ----------------------------------------------------------------------------

; TODO: Add unit tests! Check all of R4RS. Everything working correctly?
; TODO: vector and string functions are eerily similar...
; TODO: or, Named let, do loops, let*, letrec...

; TODO: Protect ALL procedures from changes by the user. If someone
; re-defines the flip procedure, string-append might stop working...

; TODO: As of now, deviations from R4RS chapter 6 are:
; - (append) created lists do not share the last argument, dotted lists
;   don't work yet
; - Distinction between integer and real, not between exact and inexact
; - Numerical operations depend on the runtime. Minimal numeric tower here
;   consisting of fixnum and real.
; - Character procedures consider the ASCII charset only
; - (map), (for-each), (filter), (every), (any) take two arguments, not
;   an arbitrary number
; - No support for nor dependency on call/cc
; - Ports (chapter 6.10) are right out at the moment

; Functions missing completely from R4RS:
; - (numerator)
; - (denominator)
; - (floor)
; - (ceiling)
; - (truncate)
; - (round)
; - (rationalize)
; - (exp)
; - (expt)
; - (log)
; - (sin)
; - (cos)
; - (tan)
; - (asin)
; - (acos)
; - (atan)
; - (make-rectangular)
; - (make-polar)
; - (real-part)
; - (imag-part)
; - (magnitude)
; - (angle)
; - (exact->inexact)
; - (inexact->exact)

(define (complex? obj) #f)
(define (rational? obj) #f)
(define exact? integer?)
(define inexact? real?)

; ----------------------------------------------------------------------------

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list . lst) lst)
(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (not x)
  (if x
      #f
      #t))

(define (> a b)
  (if (< a b)
      #f
      (not (= a b))))

(define (<= a b)
  (not (> a b)))

(define (>= a b)
  (not (< a b)))

(define (number? x)
  (if (integer? x)
      #t
      (real? x)))

(define (abs x)
  (if (positive? x)
      x
      (- 0 x)))

(define (even? x)
  (zero? (remainder x 2)))

(define (odd? x)
  (not (even? x)))

(define (error . args)
  (for-each (lambda (i) (sys:display-string (sys:object->string i #f)))
            args)
  (newline)
  (sys:exit 1))

(define (sys:sign x)
  (if (negative? x)
      -1
      1))

(define (modulo a b)
  (if (= (sys:sign a) (sys:sign b))
      (remainder a b)
      (+ b (remainder a b))))

(define (string . values)
  (list->string values))

(define (list-tail lst k)
  (if (zero? k)
      lst
      (list-tail (cdr lst) (- k 1))))

(define (list-ref lst k)
  (car (list-tail lst k)))

(define (fold f acc lst)
  (if (null? lst)
      acc
      (fold f
            (f (car lst) acc)
            (cdr lst))))

(define (reduce f ridentity lst)
  (if (null? lst)
      ridentity
      (fold f (car lst) (cdr lst))))

(define (reverse lst)
  (fold cons '() lst))

(define (length lst)
  (fold (lambda (i acc) (+ acc 1))
        0
        lst))

(define (for-each f lst)
  (fold (lambda (i acc) (f i))
        '()
        lst)
  'undefined)

(define (map f lst) ; HACK: Simple but slow
  (reverse
    (fold (lambda (i acc) (cons (f i) acc))
          '()
          lst)))

(define (filter f lst) ; HACK: Simple but slow
  (reverse
    (fold (lambda (i acc) (if (f i) (cons i acc) acc))
          '()
          lst)))

(defmacro let (lst . forms)
  (cons
    (cons 'lambda (cons (map car lst) forms))
    (map cadr lst)))

(defmacro cond list-of-forms
  (define (expand-cond lst)
    (if (null? lst)
        #f
        (if (eq? (caar lst) 'else)
            (cons 'begin
                  (cdar lst))
            (list 'if
                  (caar lst)
                  (cons 'begin
                        (cdar lst))
                  (expand-cond (cdr lst))))))
  (expand-cond list-of-forms))

(define (list? lst)
  (cond ((null? lst) #t)
        ((pair? lst) (list? (cdr lst)))
        (else #f)))

(define (every f lst)
  (cond ((null? lst) #t)
        ((f (car lst)) (every f (cdr lst)))
        (else #f)))

(define (any f lst)
  (cond ((null? lst) #f)
        ((f (car lst)) #t)
        (else (any f (cdr lst)))))

(define (sys:gcd-of-two a b)
  (if (zero? b)
      a
      (sys:gcd-of-two b (remainder a b))))

(define (sys:lcm-of-two a b)
  (/ (* a b)
     (sys:gcd-of-two a b)))

(define (gcd . args)
  (abs
    (reduce sys:gcd-of-two 0 args)))

(define (lcm . args)
  (abs
    (reduce sys:lcm-of-two 1 args)))

(define (append . lsts) ; HACK: Speed up!
  (define (iter current acc)
    (if (null? current)
        acc
        (iter (cdr current)
              (cons (car current) acc))))
  (reverse
    (fold iter '() lsts)))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(define (last lst)
  (car (last-pair lst)))

(define (find-tail f lst)
  (cond ((null? lst) #f)
        ((f (car lst)) lst)
        (else (find-tail f (cdr lst)))))

(define (find f lst)
  (cond ((null? lst) #f)
        ((f (car lst)) (car lst))
        (else (find f (cdr lst)))))

(defmacro and list-of-forms
  (if (null? list-of-forms)
      #t
      (if (null? (cdr list-of-forms))
          (car list-of-forms)
          (list 'if
                (car list-of-forms)
                (append '(and)
                        (cdr list-of-forms))
                #f))))

(define (char=? a b)
  (= (char->integer a)
     (char->integer b)))

(define (char<? a b)
  (< (char->integer a)
     (char->integer b)))

(define (char>? a b)
  (> (char->integer a)
     (char->integer b)))

(define (char<=? a b)
  (not (char>? a b)))

(define (char>=? a b)
  (not (char<? a b)))

(define (char-alphabetic? c)
  (let ((as-int (char->integer c)))
    (cond ((and (> as-int 64) (< as-int 91)) #t)
          ((and (> as-int 96) (< as-int 123)) #t)
          (else #f))))

(define (char-numeric? c)
  (let ((as-int (char->integer c)))
    (and (> as-int 47) (< as-int 58))))

(define (char-whitespace? c)
  (let ((as-int (char->integer c)))
    (cond ((= as-int 32) #t)
          ((= as-int  9) #t)
          ((= as-int 10) #t)
          ((= as-int 13) #t)
          ((= as-int 14) #t)
          (else #f))))

(define (char-upper-case? c)
  (let ((as-int (char->integer c)))
    (and (> as-int 64) (< as-int 91))))

(define (char-lower-case? c)
  (let ((as-int (char->integer c)))
    (and (> as-int 96) (< as-int 123))))

(define (char-upcase c)
  (if (char-lower-case? c)
      (integer->char (- (char->integer c) 32))
      c))

(define (char-downcase c)
  (if (char-upper-case? c)
      (integer->char (+ (char->integer c) 32))
      c))

(define (char-ci=? a b)
  (char=? (char-downcase a)
          (char-downcase b)))

(define (char-ci<? a b)
  (char<? (char-downcase a)
          (char-downcase b)))

(define (char-ci>? a b)
  (char>? (char-downcase a)
          (char-downcase b)))

(define (char-ci>=? a b)
  (not (char-ci<? a b)))

(define (char-ci<=? a b)
  (not (char-ci>? a b)))

(define (flip f)
  (lambda (a b)
    (f b a)))

(let ((original make-string))
  (set! make-string
        (lambda (size . args)
          (if (null? args)
              (original size)
              (let ((v (original size)))
                   (string-fill! v (car args))
                   v)))))

(define (string-fill! v obj)
  (define (iter i len)
    (if (= i len)
        'unspecified
        (begin
          (string-set! v i obj)
          (iter (+ i 1) len))))
  (iter 0 (string-length v)))

(define (list->string lst)
  (define (iter v di i vals)
    (string-set! v di (car vals))
    (if (zero? i)
        v
        (iter v (+ di 1) (- i 1) (cdr vals))))
  (if (null? lst)
      (make-string 0)
      (let ((v (make-string (length lst))))
        (iter v 0 (- (string-length v) 1) lst))))

(define (string-copy s)
  (define (iter dst i max)
    (if (= i max)
        dst
        (begin
          (string-set! dst i (string-ref s i))
          (iter dst (+ i 1) max))))
  (iter (make-string (string-length s))
        0
        (string-length s)))

(define (substring str start end)
  (define (iter v i di)
    (if (= i end)
        v
        (begin
          (string-set! v di (string-ref str i))
          (iter v (+ 1 i) (+ 1 di)))))
  (cond ((> start end) (error "substring: end > start"))
        ((< start 0) (error "substring: start < 0"))
        ((> end (string-length str)) (error "substring: end > length"))
        (else (iter (make-string (- end start)) start 0))))

(define (string-append a b)
  (define (iter src dst di si max)
    (if (= si max)
        dst
        (begin
          (string-set! dst di (string-ref src si))
          (iter src dst (+ di 1) (+ si 1) max))))
  (let ((ret (make-string (+ (string-length a) (string-length b)))))
    (iter a ret 0 0 (string-length a))
    (iter b ret (string-length a) 0 (string-length b))))

(define (string=? a b)
  (define (iter i max)
    (cond ((>= i max) #t)
          ((char=? (string-ref a i) (string-ref b i)) (iter (+ i 1) max))
          (else #f)))
  (if (= (string-length a) (string-length b))
      (iter 0 (string-length a))
      #f))

(define (string<? a b) ; HACK: Speed up!
  (define (iter i la lb)
    (cond ((= i la) (< i lb))
          ((= i lb) #f)
          ((char<? (string-ref a i) (string-ref b i)) #t)
          ((char=? (string-ref a i) (string-ref b i)) (iter (+ i 1) la lb))
          (else #f)))
  (iter 0 (string-length a) (string-length b)))

(define string>? (flip string<?))

(define (string>=? a b)
  (not (string<? a b)))

(define (string<=? a b)
  (not (string>? a b)))

(define (string-ci=? a b)
  (define (iter i max)
    (cond ((>= i max) #t)
          ((char-ci=? (string-ref a i) (string-ref b i)) (iter (+ i 1) max))
          (else #f)))
  (if (= (string-length a) (string-length b))
      (iter 0 (string-length a))
      #f))

(define (string-ci<? a b) ; HACK: Speed up!
  (define (iter i la lb)
    (cond ((= i la) (< i lb))
          ((= i lb) #f)
          ((char-ci<? (string-ref a i) (string-ref b i)) #t)
          ((char-ci=? (string-ref a i) (string-ref b i)) (iter (+ i 1) la lb))
          (else #f)))
  (iter 0 (string-length a) (string-length b)))

(define string-ci>? (flip string-ci<?))

(define (string-ci>=? a b)
  (not (string-ci<? a b)))

(define (string-ci<=? a b)
  (not (string-ci>? a b)))

(define (string->list s)
  (define (iter i acc)
    (if (< i 0)
        acc
        (iter (- i 1)
              (cons (string-ref s i)
                    acc))))
  (iter (- (string-length s) 1) '()))

(define (eqv? a b)
  (define (bool=? a b)
    (if a
        b
        (not b)))
  (cond ((eq? a b) #t)
        ((and (real? a) (real? b)) (= a b))
        ((and (integer? a) (integer? b)) (= a b))
        ((and (char? a) (char? b)) (char=? a b))
        ((and (boolean? a) (boolean? b)) (bool=? a b))
        (else #f)))

(define (equal? a b)
  (define (list-equal? i j)
    (cond ((and (null? i) (null? j)) #t)
          ((null? i) #f)
          ((null? j) #f)
          ((equal? (car i) (car j)) (list-equal? (cdr i) (cdr j)))
          (else #f)))
  (define (vector-equal? i j)
    (list-equal? (vector->list a) (vector->list b))) ; HACK: Speed up!
  (cond ((eqv? a b) #t)
        ((and (string? a) (string? b)) (string=? a b))
        ((and (pair? a) (pair? b)) (list-equal? a b))
        ((and (vector? a) (vector? b)) (vector-equal? a b))
        (else #f)))

(define (memq obj lst)
  (find-tail (lambda (i) (eq? i obj))
             lst))

(define (memv obj lst)
  (find-tail (lambda (i) (eqv? i obj))
             lst))

(define (member obj lst)
  (find-tail (lambda (i) (equal? i obj))
             lst))

(define (assq obj lst)
  (find (lambda (i) (eq? (car i) obj)) lst))

(define (assv obj lst)
  (find (lambda (i) (eqv? (car i) obj)) lst))

(define (assoc obj lst)
  (find (lambda (i) (equal? (car i) obj)) lst))

(define (min a b)
  (if (< a b)
      a
      b))

(define (max a b)
  (if (> a b)
      a
      b))

(let ((original min))
  (set! min
        (lambda args
          (cond ((null? args) (error "min: Called without parameters"))
                ((null? (cdr args)) (car args))
                (else (reduce original 0 args))))))

(let ((original max))
  (set! max
        (lambda args
          (cond ((null? args) (error "max: Called without parameters"))
                ((null? (cdr args)) (car args))
                (else (reduce original 0 args))))))

(define (drop-while f lst)
  (cond ((null? lst) '())
        ((f (car lst)) (drop-while f (cdr lst)))
        (else lst)))

(define (take-while f lst)
  (define (iter l acc)
    (cond ((null? l) acc)
          ((f (car l)) (iter (cdr l) (cons (car l) acc)))
          (else acc)))
  (reverse (iter lst '())))

(define (take lst i)
  (define (iter l totake acc)
    (cond ((null? l) acc)
          ((zero? totake) acc)
          (else (iter (cdr l) (- totake 1) (cons (car l) acc)))))
  (reverse (iter lst i '())))

(define drop list-tail)

(define (range from to)
  (define (iter i acc)
    (if (> from i)
        acc
        (iter (- i 1) (cons i acc))))
  (iter to '()))

(let ((original make-vector))
  (set! make-vector
        (lambda (size . args)
          (if (null? args)
              (original size)
              (let ((v (original size)))
                   (vector-fill! v (car args))
                   v)))))

(define (vector-fill! v obj)
  (define (iter i len)
    (if (= i len)
        'unspecified
        (begin
          (vector-set! v i obj)
          (iter (+ i 1) len))))
  (iter 0 (vector-length v)))

(define (list->vector lst) ; HACK: Speed up!
  (define (iter v i vals)
    (vector-set! v i (car vals))
    (if (zero? i)
        v
        (iter v (- i 1) (cdr vals))))
  (if (null? lst)
      (make-vector 0)
      (let ((v (make-vector (length lst))))
        (iter v (- (vector-length v) 1) (reverse lst)))))

(define (vector . lst)
  (list->vector lst))

(define (vector->list v)
  (define (iter i acc)
    (if (< i 0)
        acc
        (iter (- i 1)
              (cons (vector-ref v i)
                    acc))))
  (iter (- (vector-length v) 1) '()))

(let ((original +))
  (set! +
        (lambda args 
          (fold original 0 args))))

(let ((original *))
  (set! *
        (lambda args
          (fold original 1 args))))

(let ((original -))
  (set! -
        (lambda args
          (cond ((null? args) (error "-: Called without parameters"))
                ((null? (cdr args)) (original 0 (car args)))
                (else (reduce (flip original)
                              0
                              args))))))

(let ((original /))
  (set! /
        (lambda args
          (cond ((null? args) (error "/: Called without parameters"))
                ((null? (cdr args)) (original 1 (car args)))
                (else (reduce (flip original)
                              0
                              args))))))

(let ((original string-append))
  (set! string-append
        (lambda args
          (fold (flip original)
                ""
                args))))

(defmacro delay (expression)
  (list 'let
        '((##forced #f)
          (##forced_value #f))
        (list 'lambda
              '()
              (list 'if '##forced
                        #t
                        (list 'begin
                              (list 'set! '##forced_value expression)
                              '(set! ##forced #t)))
              '##forced_value)))

(define (force promise)
  (promise))

(define (sys:make-string-writer)
  (let ((value '())
        (characters 0))
    (define (add-char c)
      (set! characters (+ characters 1))
      (set! value (cons c value)))
    (define (write-char c)
      (let ((as-int (char->integer c)))
        (cond ((= as-int 32) (add-string "#\space"))
              ((= as-int  9) (add-string "#\tab"))
              ((= as-int 10) (add-string "#\newline"))
              ((= as-int 13) (add-string "#\cr"))
              ((< as-int 32) (add-char #\.))
              (else (add-char c)))))
    (define (write-string-char c)
      (let ((as-int (char->integer c)))
        (cond ((= as-int 92) (add-string "\\\\"))
              ((= as-int  9) (add-string "\\t"))
              ((= as-int 10) (add-string "\\n"))
              ((= as-int 13) (add-string "\\r"))
              (else (add-char c)))))
    (define (write-string s)
      (define (iter i max)
        (write-string-char (string-ref s i))
        (if (= max i)
            'undefined
            (iter (+ i 1) max)))
      (add-char #\")
      (iter 0 (- (string-length s) 1))
      (add-char #\"))
    (define (add-string s)
      (define (iter i max)
        (add-char (string-ref s i))
        (if (= max i)
            'undefined
            (iter (+ i 1) max)))
      (iter 0 (- (string-length s) 1)))
    (define (add-number n)
      (add-string (sys:numtostr n 10)))
    (define (get-value)
      (define (iter lst dst count)
        (string-set! dst count (car lst))
        (if (zero? count)
            dst
            (iter (cdr lst) dst (- count 1))))
        (let ((v (make-string characters)))
          (iter value v (- characters 1))))
    (define (get-reverse)
      (define (iter lst dst di count)
        (string-set! dst di (car lst))
        (if (zero? count)
            dst
            (iter (cdr lst) dst (+ di 1) (- count 1))))
        (let ((v (make-string characters)))
          (iter value v 0 (- characters 1))))
    (lambda (command)
      (cond ((eq? command 'add-char) add-char)
            ((eq? command 'write-char) write-char)
            ((eq? command 'add-string) add-string)
            ((eq? command 'write-string) write-string)
            ((eq? command 'add-number) add-number)
            ((eq? command 'get) get-value)
            ((eq? command 'get-reverse) get-reverse)
            (else (error "string-writer: Unknown command"))))))

(define (sys:make-string-reader str)
  (let ((len (string-length str))
        (position 0))
    (define (eof?)
      (>= position len))
    (define (assert-not-eof)
      (if (eof?)
          (error "string-reader: Nothing left to read")
          #t))
    (define (peek-char)
      (assert-not-eof)
      (string-ref str position))
    (define (get-char)
      (let ((ret (peek-char)))
        (set! position (+ position 1))
        ret))
    (define (get-identifier)
      'TODO)
    (define (skip-whitespace)
      (cond ((eof?) 'eof)
            ((char-whitespace? (peek-char)) (get-char) (skip-whitespace))
            (else 'ok)))
    (define (skip-line)
      (cond ((eof?) 'eof)
            ((char=? #\newline (get-char)) 'ok)
            (else (skip-line))))
    (lambda (command)
      (cond ((eq? command 'eof?) eof?)
            ((eq? command 'peek-char) peek-char)
            ((eq? command 'get-char) get-char)
            ((eq? command 'get-identifier) get-identifier)
            ((eq? command 'skip-whitespace) skip-whitespace)
            ((eq? command 'skip-line) skip-line)
            (else (error "string-reader: Unknown command"))))))

(define (sys:add-pair-to-string-writer obj stream readable)
  (define (iter i)
    (sys:add-object-to-string-writer (car i) stream readable)
    (cond ((null? (cdr i)) 'done)
          ((pair? (cdr i)) ((stream 'add-char) #\space) (iter (cdr i)))
          (else
            ((stream 'add-string) " . ") 
            (sys:add-object-to-string-writer (cdr i) stream readable)
            'done)))
  ((stream 'add-char) #\()
  (iter obj)
  ((stream 'add-char) #\)))

(define (sys:add-object-to-string-writer obj stream readable)
  (cond ((null? obj) ((stream 'add-string) "()"))
        ((boolean? obj) ((stream 'add-string) (if obj "#t" "#f")))
        ((number? obj) ((stream 'add-number) obj))
        ((symbol? obj) ((stream 'add-string) (symbol->string obj)))
        ((char? obj) ((stream (if readable 'write-char 'add-char)) obj))
        ((string? obj) ((stream (if readable 'write-string 'add-string)) obj))
        ((pair? obj) (sys:add-pair-to-string-writer obj stream readable))
        ((vector? obj) ((stream 'add-char) #\#)
                       (sys:add-object-to-string-writer (vector->list obj)
                                                       stream
                                                       readable))
        ((procedure? obj) ((stream 'add-string) "<procedure>"))
        (else (error "Unable to create readable representation of object"))))

(define (sys:object->string obj readable)
  (let ((stream (sys:make-string-writer)))
    (sys:add-object-to-string-writer obj stream readable)
    ((stream 'get))))

(define (display . args)
  (for-each (lambda (i) (sys:display-string (sys:object->string i #f)))
            args))

(define (write . args)
  (for-each (lambda (i) (sys:display-string (sys:object->string i #t)))
            args))

(define (newline) (display "\n"))

(define (string->number n . rest)
  (let ((ret (sys:strtonum n (if (pair? rest) (car rest) 10))))
    (if (eq? ret 'nan)
        (error "string->number: Value can not be converted to a number")
        ret)))

(define sys:digits (list->vector (string->list "0123456789abcdef")))

(define (sys:digit->char n)
  (if (< n 16)
      (vector-ref sys:digits n)
      (error "digit->char: Invalid digit")))

(define (sys:real-truncate x)
  (- x (remainder x 1)))

(define (sys:numtostr-test number base)
  (define (iter stream n max-digits)
    (if (zero? max-digits)
        'done
        (let ((q (quotient n base))
              (r (truncate (remainder n base))))
          (if (<= q 0)
              'done
              (iter stream q (- max-digits 1)))
          ((stream 'add-char) (sys:digit->char r)))))
  (define (realtostr stream n)
    (iter stream (sys:real-truncate n) -1)
    ((stream 'add-char) #\.)
    (iter stream
          (* 1000000 (remainder n 1))
          6))
  (let ((stream (sys:make-string-writer)))
    (if (negative? number)
        ((stream 'add-char) #\-)
        #t)
    (cond ((zero? number) ((stream 'add-char) #\0))
          ((real? number) (realtostr stream (abs number)))
          ((memv base '(2 8 10 16)) (iter stream (abs number) -1))
          (else (error "number->string: Invalid base, allowed are 2, 8, 10, 16")))
    ((stream 'get))))

(define (number->string n . rest)
  (if (pair? rest)
      (sys:numtostr n (car rest))
      (sys:numtostr n 10)))

(define (sys:abs<epsilon x)
  (< (abs x) *sys:epsilon*))

(define (sqrt n)
  (define (iter guess)
    (if (sys:abs<epsilon (- n (* guess guess)))
        guess
        (iter (* 0.5 (+ guess (/ n guess))))))
  (if (< n 0)
      (error "sqrt: Complex numbers not implemented yet")
      (iter 1)))

(define r (sys:make-string-reader "tasd d blu"))
((r 'skip-whitespace))
(display ((r 'get-identifier)))
(newline)
((r 'skip-whitespace))
(display ((r 'get-identifier)))
(newline)
((r 'skip-whitespace))
(display ((r 'get-identifier)))
(newline)

