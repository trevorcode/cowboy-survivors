;; title:   game title
;; author:  game developer, email, etc.
;; desc:    short description
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  scheme

(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))

(define (co/make-coroutine thunk)
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

(define (co/spawn! thunk)
  (set! *coroutines* (cons (co/make-coroutine thunk) *coroutines*)))

(define (co/pump-coroutines!)
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


(define *camera*
  (hash-table :x 0
	      :y 0
	      :target-x 10
	      :target-y 10))

(define (update-camera!)
  (set! (*camera* :x) (lerp 0.1 (*camera* :x) (*camera* :target-x)))
  (set! (*camera* :y) (lerp 0.1 (*camera* :y) (*camera* :target-y))))

(define (apply-camera x y)
  (values (+ x (*camera* :x))
	  (+ y (*camera* :y))))

(define* (draw-sprite sx sy scale angle mx my mw mh key (camera #f))
  (define sv (vector (vector -1 -1 0 0)
		     (vector 1 -1 0.999 0)
		     (vector -1 1 0 0.999)
		     (vector 1 1 0.999 0.999))) 
  (define scale-x (* (ash mw 2) scale))
  (define scale-y (* (ash mh 2) scale))
  (define camera-x (if camera (camera :x) 0))
  (define camera-y (if camera (camera :y) 0))

  (let loop ((p 0))
    (when (< p (length sv))
      (let* ((_sx (* (vector-ref sv p 0) scale-x))
	     (_sy (* (vector-ref sv p 1) scale-y))
	     (a (* -1 angle))
	     (rx (- (* _sx (cos a)) (* _sy (sin a))))
	     (ry (+ (* _sx (sin a)) (* _sy (cos a)))))
	(vector-set! sv p 0 (round (+ rx camera-x sx)))
	(vector-set! sv p 1 (round (+ ry camera-y sy)))
	(vector-set! sv p 2 (round (+ (ash mx 3) (* (vector-ref sv p 2) (ash mw 3)))))
	(vector-set! sv p 3 (round (+ (ash my 3) (* (vector-ref sv p 3) (ash mh 3)))))
	(loop (+ 1 p)))))
  (t80::trace (object->string sv))
  (t80::ttri (sv 0 0) (sv 0 1)
	     (sv 1 0) (sv 1 1)
	     (sv 2 0) (sv 2 1)
	     (sv 0 2) (sv 0 3)
	     (sv 1 2) (sv 1 3)
	     (sv 2 2) (sv 2 3) 0 key)

  (t80::ttri (sv 1 0) (sv 1 1)
	     (sv 2 0) (sv 2 1)
	     (sv 3 0) (sv 3 1)
	     (sv 1 2) (sv 1 3)
	     (sv 2 2) (sv 2 3)
	     (sv 3 2) (sv 3 3) 0 key))

(define cowboy-sprite 256)
(define cowboy-sprite 256)

(define (game-scene)
  (define cowboy (hash-table
		  :x 0
		  :y 0
		  :moving? #f))

  (define (input)

    (if (t80::btn 3) (set! (cowboy :x) (+ 1 (cowboy :x))))
    (if (t80::btn 1) (set! (cowboy :y) (+ 1 (cowboy :y))))
    (if (t80::btn 2) (set! (cowboy :x) (- (cowboy :x) 1)))
    (if (t80::btn 0) (set! (cowboy :y) (- (cowboy :y) 1)))
    
    (if (or (t80::btn 0) (t80::btn 1)
	    (t80::btn 2) (t80::btn 3))
	(set! (cowboy :moving?) #t)
	(set! (cowboy :moving?) #f)))

  (define (update)
    (hash-table-set! *camera* :target-x (cowboy :x))
    (hash-table-set! *camera* :target-y (cowboy :y))
    (update-camera!))

  (define (render cowboy)
    (t80::cls 13)
    (t80::elli (apply-camera (cowboy :x) (+ 8 (cowboy :y))) 7 3 0)
    (let ((y-offset (+ -2 (* 2 (sin (/ t 3)))))
	  (rotation (* 0.1 (sin (* t 0.1))))
	  (rotation1 0))
      (if (cowboy :moving?)
	  
	  (draw-sprite (cowboy :x) (+ y-offset (cowboy :y)) 2 (* pi rotation) 0 16 1 1 15 :camera *camera*)
	  (draw-sprite (cowboy :x) (cowboy :y) 2 0 0 16 1 1 15 :camera *camera*))))

  (lambda ()
    (input)
    (update)
    (render cowboy)
     
    ))

(define t 0)
(define scene (game-scene))

(define (TIC)
  (co/pump-coroutines!)
  (scene)
  
  (inc! t 1))

;; <TILES>
;; 000:6666666666666666666676766666676666666666666666666666666666666666
;; 001:6666666666666666666666666666666666666666666666666666666666666666
;; 002:6666666666666666666666666666666666666666666666666666666666666666
;; 003:6666666666666666666666666666666666666666666666666666666666666666
;; 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
;; 016:6666666666666666666666666676766666676666666666666666666666666666
;; 017:6666666666676766666676666666666666666666666666666666666666666666
;; 018:6666666666666666666666666666666666666666666666666666666666666666
;; 019:6666666666666666666666666666666666666666666666666666666666666666
;; 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; 032:fff00fff00f010ff0300110001113000000110ffff0310ffff0110ffff0110ff
;; </TILES>

;; <SPRITES>
;; 000:f066770f06677770f005500fff0550fff077770ff077770ff066660ff060060f
;; </SPRITES>

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
;; 000:191d1e3f455070787ac0bfc0eae5e2baaa91877459534b3a29366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

