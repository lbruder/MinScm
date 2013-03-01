; vim:lisp:et:ai

; init.scm version 2013-03-01
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
; THE SOFTWARE IS PROVIDED 'AS IS' AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

; ----------------------------------------------------------------------------
; CONFIGURATION
; ----------------------------------------------------------------------------

(define *epsilon* 0.000001) ; Precision for inexact arithmetic functions

; ----------------------------------------------------------------------------
; RUNTIME REQUIREMENTS
; ----------------------------------------------------------------------------

; Expected special forms:

; if define set! quote lambda begin

; Expected macro functionality:

; (defmacro name args (form) (form) (form))
; (defmacro name (arg arg arg) (form) (form) (form))
; (defmacro name (arg arg . rest) (form) (form) (form))

; Expected procedures:

; One parameter, working as expected:
car cdr char->integer integer->char string-length vector-length
string->symbol symbol->string make-string make-vector

; One parameter, special stuff:
type ; returns the type of the argument as a symbol
tag ; Create a special object of type 'tag from the value given
untag ; Return the value stored in a 'tag object
display-string ; takes a string to output
exit ; ends the program with the fixnum given as the program's return code
flo->str ; number -> string
str->flo ; number -> flonum or 'nan
fix->flo ; fixnum -> flonum

; Two parameters:
cons set-car! set-cdr! eq? sys:apply string-ref vector-ref fix+ fix- fix*
fix/ fix% fix< fix= flo+ flo- flo* flo/ flo< flo=

; Three parameters:
string-set! vector-set!

; Only needed until re-coded in this lib:
fix->str ; number, base -> string
str->fix ; number, base -> fixnum or 'nan

; ----------------------------------------------------------------------------
; MISSING STUFF
; ----------------------------------------------------------------------------

; - TODO: Re-defining builtins may (and very probably will) break your program
; - TODO: Add unit tests! Check all of R4RS. Everything working correctly?
; - TODO: vector and string functions are eerily similar...
; - TODO: or, named let, do loops, let*, letrec...
; - TODO: Signed numbers and rationals in reader
; - TODO: eval

; As of now, deviations from R4RS chapter 6 are:
; - (append) created lists do not share the last argument, dotted lists
;   don't work yet
; - Distinction between rational and real, not between exact and inexact
; - Numerical tower consists of fixnum => rational => flonum, no bigints or
;   complex numbers (yet)
; - Character procedures consider the ASCII charset only
; - (map), (for-each), (filter), (every), (any) take two arguments, not
;   an arbitrary number
; - No support for nor dependency on call/cc
; - No Ports (chapter 6.10) yet

; Functions missing completely from R4RS:
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
; - (inexact->exact)

; ----------------------------------------------------------------------------
; INTEGRATED COMPILER
; ----------------------------------------------------------------------------

; The function (compile code emit) implements a very basic Scheme compiler
; that reads expressions from the string given in the 'code' variable and
; calls the 'emit' procedure for every low-level statement to be output. By
; providing an appropriate emit procedure, a Scheme source can be compiled
; e.g. to C or Assembly language.

; Registers used in the virtual machine are:
; - args (list of arguments for a procedure call)
; - continue (return address for a procedure call)
; - env (pointer to active environment)
; - value (main calculating register)

; Execute like this:

; public object Run()
; {
;     programCounter = 0;
;     environmentRegister = globalEnvironment; // Containing all required procedures as stated above
;     continueRegister = -1; // Continuing to -1 means program end
;     valueRegister = null;
;     argumentsRegister = null;
;     stack.Clear();
; 
;     while (programCounter < Instructions.Count)
;     {
;         Instructions[programCounter].Execute();
;         if (programCounter == -1) break;
;     }
; 
;     if (stack.Any()) throw new Exception("Bad program: Stack not empty after last instruction");
;     if (argumentsRegister != null) throw new Exception("Bad program: Arguments register not empty after last instruction");
;     return valueRegister;
; }

; The low-level statements used are:

; args->value
; Transfer contents of args to value register.

; branch-if-true label
; Jump to label if value register contains something other than #f.

; call
; Load closure given in value register, check if number of arguments in args
; register is correct. Set env register to a new environment based on the
; one the closure has captured, then set parameter variables to the values
; given in args register. Set program counter to the first statement in the
; closure.

; continue
; Jump to the instruction indicated in the continue register.

; define-variable name
; Define a variable in the current environment.

; get-variable symbol
; If variable exists in the current environment, return its value. Otherwise
; look in the outer environment(s).

; goto label
; Set the program counter to the position given.

; init-args
; Initialize args register with ().

; label symbol
; Define a jump target.

; load-constant value
; Load a constant value (fixnum, real, boolean, (), literal string) into the
; value register.

; load-continue label
; Load the jump target given into the continue register.

; make-closure name closure-label has-rest-parameter parameter-names
; Create a new closure by capturing the current environment and place the
; newly created closure into the value register.

; push-param
; Insert the contents of the value register at the beginning of the list
; stored in the args register.

; restore-registers
; POP the contents of the args, continue, and env registers from the stack.

; save-registers
; PUSH the contents of the args, continue, and env registers onto the stack.

; set-variable symbol
; If variable exists in the current environment, set its value. Otherwise
; look in the outer environment(s).

; value->args
; Transfer contents of value to args register.

; Base -----------------------------------------------------------------------

(define (pair? x) (eq? (type x) 'pair))
(define (null? x) (eq? (type x) 'null))
(define (string? x) (eq? (type x) 'string))
(define (boolean? x) (eq? (type x) 'boolean))
(define (symbol? x) (eq? (type x) 'symbol))
(define (fixnum? x) (eq? (type x) 'fixnum))
(define (flonum? x) (eq? (type x) 'flonum))
(define (procedure? x) (eq? (type x) 'procedure))
(define (vector? x) (eq? (type x) 'vector))
(define (char? x) (eq? (type x) 'char))
(define (tagged? x) (eq? (type x) 'tag))

(define (make-tagged-value tag-symbol value)
  (tag (cons tag-symbol value)))

(define (tagged-type? obj tag-symbol)
  (if (tagged? obj)
      (eq? tag-symbol (car (untag obj)))
      #f))

(define (get-tagged-value obj tag-symbol)
  (if (tagged-type? obj tag-symbol)
      (cdr (untag obj))
      (error "get-tagged-value: Object is not of desired type")))

(define (type-name obj)
  (if (tagged? obj)
      (car (untag obj))
      (type obj)))

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

(define (apply f lst) (sys:apply f lst)) ; Turn the special form into a procedure
(define (list . lst) lst)

(define (not x)
  (if x
      #f
      #t))

(define (flip f)
  (lambda (a b)
    (f b a)))

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

; Bigints --------------------------------------------------------------------

; TODO: Implement

(define integer? fixnum?)
(define bi->str fix->str)
(define str->bi str->fix)
(define bi< fix<)
(define bi= fix=)
(define bi+ fix+)
(define bi- fix-)
(define bi* fix*)
(define bi/ fix/)
(define bi% fix%)
(define bi->flo fix->flo)

; Rationals ------------------------------------------------------------------

(define (gcd-of-two a b)
  (if (bi= 0 b)
      a
      (gcd-of-two b (bi% a b))))

(define (lcm-of-two a b)
  (bi/ (bi* a b)
       (gcd-of-two a b)))

(define (gcd . args)
  (abs
    (reduce gcd-of-two 0 args)))

(define (lcm . args)
  (abs
    (reduce lcm-of-two 1 args)))

(define (fraction? n)
  (tagged-type? n 'fraction))

(define (rational? n)
  (if (fraction? n)
      #t
      (integer? n)))

(define (numerator n)
  (if (fraction? n)
      (car (get-tagged-value n 'fraction))
      n))

(define (denominator n)
  (if (fraction? n)
      (cdr (get-tagged-value n 'fraction))
      1))

(define (fraction->string n)
  (if (fraction? n)
      (let ((stream (make-string-writer)))
        ((stream 'add-number) (numerator n))
        ((stream 'add-char) #\/)
        ((stream 'add-number) (denominator n))
        ((stream 'get)))
      (error "fraction->string: Object is not of fraction type")))

(define (rational->string n base)
  (if (fraction? n)
      (if (bi= base 10)
          (fraction->string n)
          (error "rational->string: Only base 10 allowed for fractions")) ; TODO
      (bi->str n base)))

(define (rational->flonum n)
  (if (integer? n)
      (bi->flo n)
      (flo/ (bi->flo (numerator n))
            (bi->flo (denominator n)))))

(define (make-fraction a b)
  (if (and (integer? a) (integer? b))
      (let ((g (gcd a b)))
        (if (bi= b g)
            (bi/ a g)
            (make-tagged-value 'fraction
                               (cons (bi/ a g) ; TODO: Store sign in numerator. Denominator is always positive!
                                     (bi/ b g)))))
      (error "make-fraction: Expected two integer values, got " (type-name a) " and " (type-name b))))

(define (frac+ a b)
  (make-fraction (bi+ (bi* (numerator a) (denominator b))
                      (bi* (numerator b) (denominator a)))
                 (bi* (denominator a) (denominator b))))

(define (frac- a b)
  (make-fraction (bi- (bi* (numerator a) (denominator b))
                      (bi* (numerator b) (denominator a)))
                 (bi* (denominator a) (denominator b))))

(define (frac* a b)
  (make-fraction (bi* (numerator a) (numerator b))
                 (bi* (denominator a) (denominator b))))

(define (frac/ a b)
  (make-fraction (bi* (numerator a) (denominator b))
                 (bi* (denominator a) (numerator b))))

(define (frac< a b)
  (bi< (bi* (numerator a) (denominator b))
       (bi* (numerator b) (denominator a))))

(define (frac= a b)
  (bi= (bi* (numerator a) (denominator b))
       (bi* (numerator b) (denominator a))))

; Numerical tower ------------------------------------------------------------

(define (real? x)
  (if (flonum? x)
      #t
      (rational? x)))

(define exact? rational?) ; TODO
(define (inexact? obj) (not (exact? obj))) ; TODO
(define complex? real?) ; TODO
(define number? complex?) ; TODO

(define (exact->inexact x)
  (if (exact? x)
      (rational->flonum x)
      x))

(define (+ a b)
  (cond ((flonum? a) (flo+ a (exact->inexact b)))
        ((flonum? b) (flo+ (exact->inexact a) b))
        (else (frac+ a b))))

(define (- a b)
  (if (flonum? a)
      (flo- a (exact->inexact b))
      (if (flonum? b)
          (flo- (exact->inexact a) b)
          (frac- a b))))

(define (* a b)
  (if (flonum? a)
      (flo* a (exact->inexact b))
      (if (flonum? b)
          (flo* (exact->inexact a) b)
          (frac* a b))))

(define (/ a b)
  (if (flonum? a)
      (flo/ a (exact->inexact b))
      (if (flonum? b)
          (flo/ (exact->inexact a) b)
          (frac/ a b))))

(define (< a b)
  (if (flonum? a)
      (flo< a (exact->inexact b))
      (if (flonum? b)
          (flo< (exact->inexact a) b)
          (frac< a b))))

(define (= a b)
  (if (flonum? a)
      (flo= a (exact->inexact b))
      (if (flonum? b)
          (flo= (exact->inexact a) b)
          (frac= a b))))

(define (quotient a b)
  (if (integer? a)
      (if (integer? b)
          (bi/ a b)
          (error "quotient: Expected integer argument, got " (type-name b)))
      (error "quotient: Expected integer argument, got " (type-name a))))

(define (remainder a b)
  (if (integer? a)
      (if (integer? b)
          (bi% a b)
          (error "remainder: Expected integer argument, got " (type-name b)))
      (error "remainder: Expected integer argument, got " (type-name a))))

(define (zero? x)
  (= x 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (> a b)
  (if (< a b)
      #f
      (not (= a b))))

(define (<= a b)
  (not (> a b)))

(define (>= a b)
  (not (< a b)))

(define (abs x)
  (if (positive? x)
      x
      (- 0 x)))

(define (even? x)
  (zero? (remainder x 2)))

(define (odd? x)
  (not (even? x)))

(define (sign x)
  (if (negative? x)
      -1
      1))

(define (modulo a b)
  (if (= (sign a) (sign b))
      (remainder a b)
      (+ b (remainder a b))))

(define (min a b)
  (if (< a b)
      a
      b))

(define (max a b)
  (if (> a b)
      a
      b))

; Character procedures -------------------------------------------------------

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

(define (char-digit-or-period? c)
  (if (char-numeric? c)
      #t
      (char=? c #\.)))

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

; Vector procedures ----------------------------------------------------------

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

; String procedures ----------------------------------------------------------

(define (numtostr n base)
  (if (rational? n)
      (rational->string n base)
      (flo->str n))) ; TODO

(define (strtonum str base)
  (let ((ret (str->bi str base))) ; TODO: Rationals
    (if (eq? ret 'nan)
        (str->flo str)
        ret)))

(define (string->number n . rest)
  (let ((ret (strtonum n (if (pair? rest) (car rest) 10))))
    (if (eq? ret 'nan)
        (error "string->number: Value can not be converted to a number")
        ret)))

(define (number->string n . rest)
  (if (pair? rest)
      (numtostr n (car rest))
      (numtostr n 10)))

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

(define (string . values)
  (list->string values))

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

; Promises -------------------------------------------------------------------

(define (make-promise f)
  (let ((value #f)
        (forced #f))
    (make-tagged-value 'promise
                       (lambda ()
                         (if forced
                             value
                             (begin
                               (set! value (f))
                               (set! forced #t)
                               value))))))

(define (promise? obj)
  (tagged-type? obj 'promise))

(define (force obj)
  (if (promise? obj)
      ((get-tagged-value obj 'promise))
      obj))

(defmacro delay (expression)
  (list 'make-promise
        (list 'lambda 
              '()
              expression)))

; TODO categorize ------------------------------------------------------------

(define (error . args)
  (for-each (lambda (i) (display-string (object->string i #f)))
            args)
  (newline)
  (exit 1))

(define (list-tail lst k)
  (if (zero? k)
      lst
      (list-tail (cdr lst) (- k 1))))

(define (list-ref lst k)
  (car (list-tail lst k)))

(define (length lst)
  (fold (lambda (i acc) (+ acc 1))
        0
        lst))

(define (abs<epsilon x)
  (< (abs x) *epsilon*))

(define (sqrt n)
  (define (iter guess)
    (if (abs<epsilon (- n (* guess guess)))
        guess
        (iter (* 0.5 (+ guess (/ n guess))))))
  (if (< n 0)
      (error "sqrt: Complex numbers not implemented yet")
      (iter 1.0)))

(define (eqv? a b)
  (define (bool=? a b)
    (if a
        b
        (not b)))
  (cond ((eq? a b) #t)
        ((and (number? a) (number? b)) (= a b))
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

(define (sort x f) ; HACK: Speed up!
  (cond ((null? x) x)
        ((null? (cdr x)) x)
        (else
          (let ((pivot (car x)))
            (let ((part1 (filter (lambda (i) (f i pivot)) (cdr x)))
                  (part2 (filter (lambda (i) (not (f i pivot))) (cdr x))))
              (append
                (sort part1 f)
                (list pivot)
                (sort part2 f)))))))

(define (dotted-list? lst)
  (if (null? lst)
      #f
      (if (pair? lst)
          (dotted-list? (cdr lst))
          #t)))

(define (make-proper-list lst)
  (define (iter i acc)
    (cond ((pair? i) (iter (cdr i) (cons (car i) acc)))
          ((null? i) acc)
          (else (cons i acc))))
  (reverse (iter lst '())))

; Augment operators to take an arbitrary number of arguments -----------------

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

; Writer ----------------------------------------------------------------------

(define (make-string-writer)
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
      (if (zero? (string-length s))
          'undefined
          (iter 0 (- (string-length s) 1))))
    (define (add-number n)
      (add-string (numtostr n 10)))
    (define (get-value)
      (define (iter lst dst count)
        (string-set! dst count (car lst))
        (if (zero? count)
            dst
            (iter (cdr lst) dst (- count 1))))
        (if (zero? characters)
            ""
           (let ((v (make-string characters)))
             (iter value v (- characters 1)))))
    (define (get-reverse)
      (define (iter lst dst di count)
        (string-set! dst di (car lst))
        (if (zero? count)
            dst
            (iter (cdr lst) dst (+ di 1) (- count 1))))
        (if (zero? characters)
            ""
           (let ((v (make-string characters)))
             (iter value v 0 (- characters 1)))))
    (lambda (command)
      (cond ((eq? command 'add-char) add-char)
            ((eq? command 'write-char) write-char)
            ((eq? command 'add-string) add-string)
            ((eq? command 'write-string) write-string)
            ((eq? command 'add-number) add-number)
            ((eq? command 'get) get-value)
            ((eq? command 'get-reverse) get-reverse)
            (else (error "string-writer: Unknown command"))))))

(define (add-pair-to-string-writer obj stream readable)
  (define (iter i)
    (add-object-to-string-writer (car i) stream readable)
    (cond ((null? (cdr i)) 'done)
          ((pair? (cdr i)) ((stream 'add-char) #\space) (iter (cdr i)))
          (else
            ((stream 'add-string) " . ") 
            (add-object-to-string-writer (cdr i) stream readable)
            'done)))
  ((stream 'add-char) #\()
  (iter obj)
  ((stream 'add-char) #\)))

(define (add-object-to-string-writer obj stream readable)
  (cond ((null? obj) ((stream 'add-string) "()"))
        ((boolean? obj) ((stream 'add-string) (if obj "#t" "#f")))
        ((fraction? obj) ((stream 'add-string) (fraction->string obj)))
        ((number? obj) ((stream 'add-number) obj))
        ((symbol? obj) ((stream 'add-string) (symbol->string obj)))
        ((char? obj) ((stream (if readable 'write-char 'add-char)) obj))
        ((string? obj) ((stream (if readable 'write-string 'add-string)) obj))
        ((pair? obj) (add-pair-to-string-writer obj stream readable))
        ((vector? obj) ((stream 'add-char) #\#)
                       (add-object-to-string-writer (vector->list obj)
                                                       stream
                                                       readable))
        ((procedure? obj) ((stream 'add-string) "<procedure>"))
        (else (error "Unable to create readable representation of object"))))

(define (object->string obj readable)
  (let ((stream (make-string-writer)))
    (add-object-to-string-writer obj stream readable)
    ((stream 'get))))

; Output functions ------------------------------------------------------------

(define (display . args)
  (for-each (lambda (i) (display-string (object->string i #f)))
            args))

(define (write . args)
  (for-each (lambda (i) (display-string (object->string i #t)))
            args))

(define (newline) (display "\n"))

(define (print . args)
  (write args)
  (newline))

; Reader ----------------------------------------------------------------------

(define (make-string-reader str)
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
    (define (get-identifier init)
      (let ((out (make-string-writer)))
        ((out 'add-string) init)
        (define (iter)
          (if (eof?)
              ((out 'get))
              (let ((c (peek-char)))
                (cond ((char=? c #\)) ((out 'get)))
                      ((char-whitespace? c) ((out 'get)))
                      (else ((out 'add-char) (get-char))
                            (iter))))))
        (if (and (string=? init "") (char=? (peek-char) #\)))
            (begin
              (get-char)
              ")")
            (iter))))
    (define (get-quoted-string)
      (get-char) ; skip opening quote
      (let ((out (make-string-writer)))
        (define (iter)
          (let ((c (get-char)))
            (cond ((char=? c #\") 'done)
                  ((char=? c #\\) (set! c (get-char))
                                  (cond ((char=? c #\n) ((out 'add-char) #\newline) (iter))
                                        ((char=? c #\r) ((out 'add-char) #\cr) (iter))
                                        ((char=? c #\t) ((out 'add-char) #\tab) (iter))
                                        (else ((out 'add-char) c) (iter))))
                  (else ((out 'add-char) c)
                        (iter)))))
        (iter)
        ((out 'get))))
    (define (skip-whitespace)
      (cond ((eof?) 'eof)
            ((char-whitespace? (peek-char)) (get-char) (skip-whitespace))
            (else 'ok)))
    (define (skip-line)
      (cond ((eof?) 'eof)
            ((char=? #\newline (get-char)) 'ok)
            (else (skip-line))))
    (define (get-character)
      (let ((c (get-char)))
        (if (char-alphabetic? c)
            (let ((name (get-identifier (string c))))
              (cond ((= 1 (string-length name)) (string-ref name 0))
                    ((string=? name "newline") #\newline)
                    ((string=? name "cr") #\cr)
                    ((string=? name "tab") #\tab)
                    ((string=? name "space") #\space)
                    (else (error "Read error: Invalid character name"))))
            c)))
    (define (get-special)
      (get-char) ; leading #
      (cond ((char=? (peek-char) #\() (list->vector (get-list)))
            ((char=? (peek-char) #\\) (get-char) (get-character))
            (else (get-symbol-or-number "#"))))
    (define (get-symbol-or-number init)
      (let ((sym (get-identifier init)))
        (cond ((string=? sym "#t") #t)
              ((string=? sym "#f") #f)
              ((string=? sym ".") '.)
              ((every char-digit-or-period? (string->list sym)) (string->number sym))
              (else (string->symbol sym)))))
    (define (get-list)
      (define (iter current ret)
        (let ((o (get-object #t)))
          (cond ((eq? o (string->symbol ")")) ret)
                ((and (eq? o '.) (null? current))
                 (error "Read error: Invalid dotted list"))
                ((eq? o '.) (set-cdr! current (get-object #t))
                            (if (eq? (get-object #t) (string->symbol ")"))
                                ret
                                (error "Read error: Invalid dotted list")))
                (else (let ((newPair (cons o '())))
                        (if (null? current)
                            (set! ret newPair)
                            (set-cdr! current newPair))
                        (iter newPair ret))))))
      (get-char) ; leading (
      (iter '() '()))
    (define (get-object throw-on-eof)
      (skip-whitespace)
      (if (eof?)
          (if throw-on-eof
              (error "string-reader: Unexpected end of input stream")
              'eof)
          (let ((c (peek-char)))
            (cond ((char=? c #\;) (skip-line) (get-object throw-on-eof))
                  ((char=? c #\') (get-char) (list 'quote (get-object throw-on-eof)))
                  ((char=? c #\() (get-list))
                  ((char=? c #\") (get-quoted-string))
                  ((char=? c #\#) (get-special))
                  (else (get-symbol-or-number ""))))))
    (lambda (command)
      (cond ((eq? command 'eof?) eof?)
            ((eq? command 'peek-char) peek-char)
            ((eq? command 'get-char) get-char)
            ((eq? command 'get-object) get-object)
            ((eq? command 'skip-whitespace) skip-whitespace)
            ((eq? command 'skip-line) skip-line)
            (else (error "string-reader: Unknown command"))))))

(define (read s)
  (let ((stream (make-string-reader s)))
    ((stream 'get-object) #f)))

; Compiler --------------------------------------------------------------------

(define (compile code emit)
  (define make-label '())
  (let ((label 0))
    (set! make-label
          (lambda ()
            (set! label (+ label 1))
            (string->symbol
              (string-append "label_" (number->string label))))))
  (define (compile-funcall form tail-position)
    (if tail-position
        'dummy
        (emit 'save-registers))
    (emit 'init-args)
    (for-each (lambda (arg)
                (compile-form arg #f)
                (emit 'push-param))
              (reverse (cdr form)))
    (compile-form (car form) #t)
    (if tail-position
        (emit 'call)
        (let ((continue-label (make-label)))
          (emit 'load-continue continue-label)
          (emit 'call)
          (emit 'label continue-label)
          (emit 'restore-registers))))
  (define (compile-begin-special-form form tail-position)
    (define (iter i)
      (if (pair? i)
          (begin
            (compile-form (car i) (and tail-position (null? (cdr i))))
            (iter (cdr i)))
          'done))
    (iter (cdr form)))
  (define (compile-define-variable-special-form form tail-position)
    (compile-form (caddr form) #f)
    (emit 'define-variable (cadr form)))
  (define  (compile-define-procedure-special-form form tail-position)
    (let ((name (caadr form))
          (parameter-names (make-proper-list (cdadr form)))
          (has-rest-parameter (dotted-list? (cadr form)))
          (closure-label (make-label))
          (after-closure-label (make-label)))
      (emit 'make-closure name closure-label has-rest-parameter parameter-names)
      (emit 'define-variable name)
      (emit 'goto after-closure-label)
      (emit 'label closure-label)
      (compile-begin-special-form (cdr form) #t)
      (emit 'continue)
      (emit 'label after-closure-label)))
  (define (compile-define-special-form form tail-position)
    (if (and (= 3 (length form)) (symbol? (cadr form)))
        (compile-define-variable-special-form form tail-position)
        (if (and (>= (length form) 3) (pair? (cadr form)))
            (compile-define-procedure-special-form form tail-position)
            (error "Invalid define form"))))
  (define (compile-defmacro-special-form form tail-position)
    (error "TODO: No macro support yet!"))

    ; private void HandleMacros(ref object obj)
    ; {
    ;     if (obj == null) return;
    ;     if (!(obj is Pair)) return;
    ;     if (!(((Pair)obj).First is Symbol)) return;
    ;     var form = ((Pair)obj).ToList();
    ; 
    ;     if (form[0].ToString() == "defmacro")
    ;     {
    ;         if (!(form[1] is Symbol)) throw new SchemeException("Invalid defmacro form: Name must be a symbol");
    ;         string name = "macro##" + form[1] + "##";
    ;         obj = new Pair(Symbol.FromString("define"), new Pair(new Pair(Symbol.FromString(name), form[2]), ((Pair)((Pair)((Pair)obj).Second).Second).Second));
    ;         return;
    ;     }
    ; 
    ;     while (true) if (!ExpandMacros(ref obj)) break;
    ; }
    ; 
    ; private bool ExpandMacros(ref object obj)
    ; {
    ;     if (obj == null) return false;
    ;     if (!(obj is Pair)) return false;
    ;     if (((Pair)obj).First.ToString() == "quote") return false;
    ;     for (object i = obj; i is Pair; i = ((Pair)i).Second) if (ExpandMacros(ref ((Pair)i).First)) return true;
    ; 
    ;     Symbol o1 = ((Pair)obj).First as Symbol;
    ;     if (o1 == null) return false;
    ; 
    ;     Symbol macroSymbol = Symbol.FromString("macro##" + o1 + "##");
    ;     if (!machine.HasVariable(macroSymbol)) return false;
    ; 
    ;     int nextPC = machine.ProgramSize;
    ;     compiler.Compile(new Pair(macroSymbol, Pair.FromEnumerable(((Pair)((Pair)obj).Second).Select(i => new Pair(Symbol.FromString("quote"), new Pair(i, null))))));
    ;     obj = machine.Run(nextPC);
    ; 
    ;     return true;
    ; }

  (define (compile-if-special-form form tail-position)
    (if (= 4 (length form))
        (let ((true-label (make-label))
              (next-label (make-label)))
          (compile-form (cadr form) #f) ; Condition
          (emit 'branch-if-true true-label)
          (compile-form (cadddr form) tail-position) ; Else-Part
          (emit 'goto next-label)
          (emit 'label true-label)
          (compile-form (caddr form) tail-position) ; Else-Part
          (emit 'label next-label))
        (error "Invalid if form: Expected 3 parameters")))
  (define (compile-lambda-special-form form tail-position)
    (if (< (length form) 3)
        (error "Invalid lambda form")
        (let ((parameter-names '())
              (has-rest-parameter #f)
              (closure-label (make-label))
              (after-closure-label (make-label)))
          (cond ((symbol? (cadr form)) (set! parameter-names (list (cadr form)))
                                       (set! has-rest-parameter #t))
                ((null? (cadr form)) 'nothing-to-do)
                ((pair? (cadr form)) (set! parameter-names (make-proper-list (cadr form)))
                                     (set! has-rest-parameter (dotted-list? (cadr form))))
                (else (error "Invalid lambda form")))
          (emit 'make-closure "lambda" closure-label has-rest-parameter parameter-names)
          (emit 'goto after-closure-label)
          (emit 'label closure-label)
          (compile-begin-special-form (cdr form) #t)
          (emit 'continue)
          (emit 'label after-closure-label))))
  (define (compile-quoted-object o)
    (cond ((symbol? o) (emit 'load-constant o))
          ((pair? o) (compile-quoted-list o)) ; TODO: Vectors!
          (else (emit 'load-constant o))))
  (define (compile-quoted-list lst)
    (emit 'save-registers)
    (emit 'init-args)
    (for-each (lambda (o)
                (compile-quoted-object o)
                (emit 'push-param))
              (reverse lst))
    (emit 'args->value)
    (emit 'restore-registers))
  (define (compile-quote-special-form form tail-position)
    (if (= 2 (length form))
        (compile-quoted-object (cadr form))
        (error "Invalid quote form")))
  (define (compile-set-special-form form tail-position)
    (if (= 3 (length form))
        (if (symbol? (cadr form))
            (begin
              (compile-form (caddr form) #f)
              (emit 'set-variable (cadr form)))
            (error "Invalid set! form: Value to set is not a symbol"))
        (error "Invalid set! form: Expected 2 parameters")))
  (define (compile-apply-special-form form tail-position)
    (if (= 3 (length form))
        (begin
          (if tail-position
              'dummy
              (emit 'save-registers))
          (compile-form (caddr form) #f)
          (emit 'value->args)
          (compile-form (cadr form) #f)
          (if tail-position
              (emit 'call)
              (let ((continue-label (make-label)))
                (emit 'load-continue continue-label)
                (emit 'call)
                (emit 'label continue-label)
                (emit 'restore-registers))))
        (error "Invalid apply form: Expected 2 parameters")))
  (define (compile-funcall-or-special-form form tail-position)
    (if (symbol? (car form))
        (cond ((eq? (car form) 'begin)     (compile-begin-special-form    form tail-position))
              ((eq? (car form) 'define)    (compile-define-special-form   form tail-position))
              ((eq? (car form) 'defmacro)  (compile-defmacro-special-form form tail-position))
              ((eq? (car form) 'if)        (compile-if-special-form       form tail-position))
              ((eq? (car form) 'lambda)    (compile-lambda-special-form   form tail-position))
              ((eq? (car form) 'quote)     (compile-quote-special-form    form tail-position))
              ((eq? (car form) 'set!)      (compile-set-special-form      form tail-position))
              ((eq? (car form) 'sys:apply) (compile-apply-special-form    form tail-position))
              (else (compile-funcall form tail-position)))
        (compile-funcall form tail-position)))
  (define (compile-form form tail-position)
    (cond ((symbol? form) (emit 'get-variable form))
          ((pair? form) (compile-funcall-or-special-form form tail-position))
          (else (emit 'load-constant form))))
  (define (compiler-loop reader)
    (let ((form ((reader 'get-object) #f)))
      (if (and (eq? form 'eof) ((reader 'eof?)))
          'eof
          (begin
            (compile-form form #f)
            (compiler-loop reader)))))
  (compiler-loop (make-string-reader code)))

; Unit tests ------------------------------------------------------------------

(define (perform-assertion name condition)
  (if condition
      'ok
      (display (string-append "Assertion failed: " name "\n"))))

(defmacro assert (condition)
  (list 'perform-assertion
        (object->string condition #t)
        condition))

(assert (= 1 1))
(assert (> 2 1))
(assert (< 1 2))
(assert (= (/ 12 3) 4))
(assert (> (/ 13 3) 4))
(assert (< (/ 13 3) 5))

