;; title:   game title
;; author:  game developer, email, etc.
;; desc:    short description
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  scheme

(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))

(define (make-coroutine thunk)
  (define resume #f)
  (define return #f)
  (define done #f)
  (define (yield)
    (call/cc (lambda (k)
	       (set! resume k)
	       (return #f))))
  (lambda ()
    (if done
	'done
	(call/cc
	 (lambda (k)
	   (set! return k)
	   (if resume
	       (resume #f)
	       (begin (thunk yield)
		      (set! done #t)
		      (return 'done))))))))

(define *coroutines* '())

(define (spawn! thunk)
  (set! *coroutines* (cons (make-coroutine thunk) *coroutines*)))

(define (pump-coroutines!)
  (let ((to-run *coroutines*))
    (set! *coroutines* '())
    (for-each
     (lambda (c)
       (unless (eq? (c) 'done)
	 (set! *coroutines* (cons c *coroutines*))))
     to-run)))

(define (filter predicate lst)
  (let loop ((lst lst)
	     (result '()))
    (if (null? lst)
	(reverse result)
	(let ((element (car lst)))
	  (if (predicate element)
	      (loop (cdr lst) (cons element result))
	      (loop (cdr lst) result))))))

(define (all predicate lst)
  (cond
   ((null? lst) #t)
   ((not (predicate (car lst))) #f)
   (else (all predicate (cdr lst)))))

(define (id x) x)

(define (remove-duplicates lst)
  (cond ((null? lst) '())
	((member (car lst) (cdr lst))
	 (remove-duplicates (cdr lst)))
	(else
	 (cons (car lst) (remove-duplicates (cdr lst))))))

(define (range n)
  (do ((i 0 (+ i 1))
       (acc '() (cons i acc)))
      ((= i n) (reverse acc))))

(define (lerp progress start end)
  (+ start (* progress (- end start))))

(define (ease-out-cubic p) (let ((f (- p 1))) (+ 1 (* f f f))))
(define (ease-in-cubic p) (* p p p))
(define (ease-in-out-cubic p)
  (if (< p 0.5)
      (* 4 p p p)
      (+ 1 (/ (* (- (* 2 p) 2) (- (* (- (* 2 p) 2)) (- (* 2 p) 2)) (- (* 2 p) 2)) 2))))

(define t 0)
(define x 96)
(define y 24)

(define (TIC)
  (if (t80::btn 0) (inc! y -1))
  (if (t80::btn 1) (inc! y 1))
  (if (t80::btn 2) (inc! x -1))
  (if (t80::btn 3) (inc! x 1))

  (t80::cls 13)
  (t80::spr (+ 1 (* (quotient (modulo t 60) 30) 2)) x y 14 3 0 0 2 2)
  (t80::print "HELLO WORLD!" 84 84)
  (inc! t 1))

;; <TILES>
;; 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
;; 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
;; 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
;; 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
;; 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; </TILES>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
;; </SFX>

;; <TRACKS>
;; 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </TRACKS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

