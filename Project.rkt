; amb macros from 'Teach Yourself Scheme in Fixnum Days', by Dorai Sitaram
; available from the plt-scheme helpdesk

; see also section 4.3 in Abelson and Sussman


;(require (lib "defmacro.ss"))

;;;; use the language Pretty Big -- r5rs will not permit 'require'

(require compatibility/defmacro)

(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
          (lambda ()
            (error "The entered boolean expression is not satisfiable.")))))


(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)
          
          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                             (lambda ()
                               (set! amb-fail +prev-amb-fail)
                               (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)
          
          (+prev-amb-fail))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define-macro bag-of
  (lambda (e)
    `(let ((+prev-amb-fail amb-fail)
           (+results '()))
       (if (call/cc
            (lambda (+k)
              (set! amb-fail (lambda () (+k #f)))
              (let ((+v ,e))
                (set! +results (cons +v +results))
                (+k #t))))
           (amb-fail))
       (set! amb-fail +prev-amb-fail)
       (reverse! +results))))


(define (an-element-of items)
  (assert (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))


(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
          (amb i (loop (+ i 1)))))))


; support code for puzzle, presented below

(define (member? e s)
  (cond ((null? s) #f)
        ((eq? e (car s)) #t)
        (else (member? e (cdr s)))))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member? (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Choose an element such that nothing we do later will result in a call to fail." The assert function is passed an argument which evaluates to false, it calls fail via (amb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; try a simple example of an or expression whose args are just symbols

(define (or-solution or-exp)
  (let ((alist (list (list (first-arg or-exp) (get-value)) (list (sec-arg or-exp) (get-value)))))
    (assert (or (cell-val-alt (car alist)) (cell-val-alt (cadr alist))))
    (assert (not (cell-val-alt (car alist))))
    (assert (not (cell-val-alt (cadr alist))))
    alist))

; Notice we can make the value of a be false because of the assert.
(define (test lst)
  (let ((vars (assign-vals lst)))
    (assert (and (lookup 'a vars) (not (lookup 'b vars))))
    vars))

;(test '(a b c d))


; Our original idea was to create a satisfiable? function that chained asserts together, but we have realized that is not what we want.
; The issue with chaining asserts together can be shown using an expression such as (NOT (NOT a)). This expression is clearly satisfiable (a is true). But if we
; chain asserts, it would look like this (assert (not (assert (not a))) which is clearly a contradiction. We are asserting that an assert must be false, which is guarenteed to fail.

; The correct thing we need to assert is (using the same expression) (assert (not (not a))). Here we are saying we don't care what needs to be true within the arguments of the expression,
; the only thing that needs to be true is the expression itself.

(define (get-value) (amb #t #f))

(define var? ; renamed atom? function
  (lambda(x) (and (not (pair? x)) (not (null? x)))))

(define (make-and l1 l2) (list l1 'AND l2))
(define (make-or l1 l2) (list l1 'OR l2))
(define (make-xor l1 l2) (list l1 'XOR l1))
(define (make-not l1) (list 'NOT l1))



(define (first-arg exp)
  (car exp))

(define (operator exp)
  (car (cdr exp)))

(define (second-arg exp)
  (car (cdr (cdr exp))))


(define (and-exp? exp) (eq? 'AND (operator exp)))
(define (or-exp? exp) (eq? 'OR (operator exp)))
(define (xor-exp? exp) (eq? 'XOR (operator exp)))
(define (not-exp? exp) (eq? 'NOT (first-arg exp)))



;

(define (contains? target lst)
  (cond ((null? lst) #f)
        ((eq? car(lst) target) #t)
        (else (contains? target (cdr lst)))))

; Returns alist
(define (assign-vals vars)
  (cond ((null? vars) '())
        (else (cons (list (car vars) (get-value)) (assign-vals (cdr vars))))))
;(assign-vals '(a b c d))

;((a 1) (b 1) () ())
; pre: target is contained within alist
(define (lookup target alist)
  (cond ((eq? (caar alist) target) (cadar alist))
        (else (lookup target (cdr alist)))))

;


(define (satisfiable? exp vars)
  (let ((al (assign-vals vars))); creates alist
    (cond ((var? exp) #t); dont know what should go here yet
          (else (assert (func exp al))))
    (display "The entered expression is satisfiable. A solution is listed below.")(newline)al))



; Don't know what to call this function as of right now
(define (func exp al)
  (cond ((var? exp) (lookup exp al))
        ((or-exp? exp) (or (func (first-arg exp) al) (func (second-arg exp) al)))
        ((xor-exp? exp) (and (or (func (first-arg exp) al) (func (second-arg exp) al)) (not (and (func (first-arg exp) al) (func (second-arg exp) al)))))
        ((and-exp? exp) (and (func (first-arg exp) al) (func (second-arg exp) al)))
        ((not-exp? exp) (not (func (operator exp) al)))))

(satisfiable? '(((NOT a) AND b) AND (c OR b)) '(a b c))




  



