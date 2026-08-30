;; title:   Cowboy Survivors
;; author:  TofuTheLoafu
;; desc:    Vampire Survivors-like but western themed with tic80
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  scheme

(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))

(define-macro (for args . body)
  (let ((loop (gensym))
	(binding (car args))
	(from (cadr args))
	(to (caddr args)))
    `(let ,loop ((,binding ,from))
       (when (< ,binding ,to)
	 ,@body
	 (,loop (+ ,binding 1))))))

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

(define (find predicate lst)
  (let loop ((lst lst))
    (if (null? lst)
	#f
	(let ((element (car lst)))
	  (if (predicate element)
	      element
	      (loop (cdr lst)))))))

(define (list->random lst)
  (let ((len (length lst)))
    (list-ref lst (random len))))

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
	      :target-y 10
	      :cx 120
	      :cy 68))

(define (update-camera!)
  (set! (*camera* :x) (lerp 0.05 (*camera* :x) (*camera* :target-x)))
  (set! (*camera* :y) (lerp 0.05 (*camera* :y) (*camera* :target-y))))

(define (camera-transform wx wy)
  (values (floor (+ (*camera* :cx) (- wx (*camera* :x))))
	  (floor (+ (*camera* :cy) (- wy (*camera* :y))))))

(define (draw-sprite sx sy scale angle mx my mw mh key)
  (define sv (vector (vector -1 -1 0 0)
		     (vector 1 -1 0.999 0)
		     (vector -1 1 0 0.999)
		     (vector 1 1 0.999 0.999))) 
  (define scale-x (* (ash mw 2) scale))
  (define scale-y (* (ash mh 2) scale))

  (let loop ((p 0))
    (when (< p (length sv))
      (let* ((_sx (* (vector-ref sv p 0) scale-x))
	     (_sy (* (vector-ref sv p 1) scale-y))
	     (a (* -1 angle))
	     (rx (- (* _sx (cos a)) (* _sy (sin a))))
	     (ry (+ (* _sx (sin a)) (* _sy (cos a)))))
	(vector-set! sv p 0 (round (+ rx sx)))
	(vector-set! sv p 1 (round (+ ry sy)))
	(vector-set! sv p 2 (round (+ (ash mx 3) (* (vector-ref sv p 2) (ash mw 3)))))
	(vector-set! sv p 3 (round (+ (ash my 3) (* (vector-ref sv p 3) (ash mh 3)))))
	(loop (+ 1 p)))))
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

(define (generate-map width)
  (let ((game-map (make-vector (* width width) 'empty))
	(center (/ (- width 1) 2.0))
	(radius (/ width 2.0)))
    (let loop ((i 0))
      (when (< i (* width width))
	(let* ((x (modulo i width))
	       (y (quotient i width))
	       (dx (- x center))
	       (dy (- y center))
	       (dist (sqrt (+ (* dx dx) (* dy dy)))))
	  (when (<= dist radius)
	    (vector-set! game-map i (list->random '(2 4))))
	  (loop (+ i 1)))))
    (hash-table
     :map game-map
     :width width
     :height width)))

(define (floor-quotient n d)
  (quotient (- n (modulo n d)) d))

(define (map-get-cell map x y) 
  (vector-ref (map :map) (+ x (* y (map :width)))))

(define (world->real-coords x y)
  (cons (* 16 x) (* 16 y)))

(define (map-render map-table camera)
  (let* ((game-map (map-table :map))
	 (map-width (map-table :width))
	 (map-height (map-table :height))
	 (cam-x (round (*camera* :x)))
	 (cam-y (round (*camera* :y)))
	 (cam-tile-x (floor-quotient cam-x 16))
         (cam-tile-y (floor-quotient cam-y 16))
	 (sub-x (modulo cam-x 16))
         (sub-y (modulo cam-y 16))
	 (screen-tile-width 18)
	 (screen-tile-height 12))

    (define (in-bounds? x y)
      (and (<= 0 x) (< x map-width)
	   (<= 0 y) (< y map-height)))

    (let loop ((i 0))
      (when (< i (* screen-tile-width screen-tile-height))
	(let* ((screen-x (modulo i screen-tile-width)) 
               (screen-y (quotient i screen-tile-width))
	       (world-x (+ cam-tile-x screen-x)) 
               (world-y (+ cam-tile-y screen-y)))
	  (when (and (in-bounds? world-x world-y)
		     (not (eq? (map-get-cell map-table world-x world-y) 'empty)))
	    (t80::spr (map-get-cell map-table world-x world-y) (- (* 16 screen-x) sub-x) (- (* 16 screen-y) sub-y) -1 1 0 0 2 2))
	  (loop (+ i 1)))))

    (let loop ((i 0))
      (when (< i (* screen-tile-width screen-tile-height))
	(let* ((screen-x (- (modulo i screen-tile-width) 1)) 
               (screen-y (- (quotient i screen-tile-width) 1))
	       (world-x (+ cam-tile-x screen-x)) 
               (world-y (+ cam-tile-y screen-y)))
	  (when (or (not (in-bounds? world-x world-y))
		    (eq? (map-get-cell map-table world-x world-y) 'empty))
	    (for-each
	     (lambda (offset)
	       (t80::spr 64
			 (+ (car offset) (- (* 16 screen-x) sub-x))
			 (+ (cadr offset) (- (* 16 screen-y) sub-y))
			 15 1 0 (caddr offset) 2 2))
	     '((-4 -4 0)
	       (4 4 0))))
	  (loop (+ i 1)))))
    ))

(define (overlapping? a b)
  (let* ((dist-x (- (a :x) (b :x)))
	 (dist-y (- (a :y) (b :y)))
	 (dist-sq (+ (* dist-x dist-x) (* dist-y dist-y)))
	 (min-dist (+ (a :radius) (b :radius))))
    (< dist-sq (* min-dist min-dist))))

(define (within? a b dist)
  (let* ((dist-x (- (a :x) (b :x)))
	 (dist-y (- (a :y) (b :y)))
	 (dist-sq (+ (* dist-x dist-x) (* dist-y dist-y))))
    (<= dist-sq (* dist dist))))

(define (direction-to from to)
  (let* ((dx (- (to :x) (from :x)))
         (dy (- (to :y) (from :y)))
         (dist-sq (+ (* dx dx) (* dy dy))))
    (if (= dist-sq 0)
        (cons 1.0 0.0)
        (let ((dist (sqrt dist-sq)))
          (cons (/ dx dist) (/ dy dist))))))

(define (collision-system cell-size)
  (define grid (hash-table))
  (define (cell-key x y)
    (cons (floor (/ x cell-size))
	  (floor (/ y cell-size))))

  (define (build-grid entities)
    (set! grid (hash-table))
    (for-each
     (lambda (e)
       (let ((key (cell-key (e :x) (e :y))))
	 (when (not (hash-table-ref grid key))
	     (hash-table-set! grid key (list)))
	 (hash-table-set! grid key (cons e (grid key)))))
     entities))

  (define neighbor-offsets '((1 . 0) (1 . 1) (0 . 1) (-1 . 1)))

  (define (find-collisions)
    (let ((collisions (list)))
      (for-each
       (lambda (grid-entry)
	 (let ((cx (car (car grid-entry)))
	       (cy (cdr (car grid-entry)))
	       (entities (cdr grid-entry)))
	   (for (i 0 (length entities))
		(for (j (+ i 1) (length entities))
		(when (overlapping? (entities i) (entities j))
		  (set! collisions (cons (cons (entities i) (entities j)) collisions)))))

	   (for-each
            (lambda (offset)
              (let ((neighbor-cell (grid (cons (+ cx (car offset)) (+ cy (cdr offset))))))
                (when neighbor-cell
                  (for-each
                   (lambda (a)
                     (for-each
                      (lambda (b)
                        (when (overlapping? a b)
                          (set! collisions (cons (cons a b) collisions))))
                      neighbor-cell))
                   entities))))
            neighbor-offsets)))
       grid)
      collisions))

  (lambda (entities)
    (build-grid entities)
    (find-collisions)))

(define (push-apart! e other)
  (when (overlapping? e other)
    (let* ((dist-x (- (e :x) (other :x)))
	   (dist-y (- (e :y) (other :y)))
	   (dist-sq (+ (* dist-x dist-x) (* dist-y dist-y)))
	   (min-dist (+ (e :radius) (other :radius)))
	   (zero-dist? (= dist-sq 0))
	   (dist (if zero-dist? 0.0 (sqrt dist-sq)))
	   (dir-x (if zero-dist? 1.0 (/ dist-x dist)))
	   (dir-y (if zero-dist? 0.0 (/ dist-y dist)))
	   (overlap (* 0.5 (- min-dist dist)))
	   (push-x (* overlap dir-x))
	   (push-y (* overlap dir-y)))
      (set! (e :x) (+ (e :x) push-x))
      (set! (e :y) (+ (e :y) push-y))
      (set! (other :x) (- (other :x) push-x))
      (set! (other :y) (- (other :y) push-y)))))

(define (dispatch-collision! collision-pair)
  (let ((a (car collision-pair))
	(b (cdr collision-pair)))
    (cond
     ((and (eq? (a :type) :enemy) (eq? (b :type) :enemy))
      (push-apart! a b))
     ((and (eq? (a :type) :player) (eq? (b :type) :enemy))
      (set! (a :hp) (- (a :hp) 1)))
     ((and (eq? (a :type) :enemy) (eq? (b :type) :player))
      (set! (b :hp) (- (b :hp) 1)))
     ((and (eq? (a :type) :projectile) (eq? (b :type) :enemy))
      (set! (b :hp) (- (b :hp) (a :damage)))
      (set! (a :destroy?) #t))

     ((and (eq? (a :type) :enemy) (eq? (b :type) :projectile))
      (set! (a :hp) (- (a :hp) (b :damage)))
      (set! (b :destroy?) #t))
     (else #f))))

(define (enemy-follow-player e)
  (when (<= (e :hp) 0)
    (set! (e :destroy?) #t))
  (let* ((dir (direction-to e (e :target)))
	 (dir-x (car dir))
	 (dir-y (cdr dir)))
    (set! (e :x) (+ (e :x) (* dir-x (/ (e :speed) 10))))
    (set! (e :y) (+ (e :y) (* dir-y (/ (e :speed) 10))))))

(define (render-entity e)
  (t80::spr (e :sprite) (camera-transform (e :x) (e :y)) 15 1 0 0 1 1))

(define (make-rat target)
    (let ((coords (world->real-coords 20 20))) 
      (hash-table
       :type :enemy
       :x (car coords)
       :y (cdr coords)
       :radius 4
       :speed 3
       :target target
       :update enemy-follow-player
       :sprite 64
       :hp 5
       :moving? #f)))

(define (make-cowboy game-map)
  (let* ((coords (world->real-coords
		  (quotient (game-map :width) 2)
		  (quotient (game-map :height) 2)))
	 (cowboy  
	  (hash-table
	   :type :player
	   :x (car coords)
	   :y (cdr coords)
	   :radius 8
	   :speed 10
	   :upgrades (list)
	   :update (lambda (x y) #f)
	   :hp 5
	   :moving? #f)))
    (set! (cowboy :upgrades) (cons (make-revolver cowboy) (cowboy :upgrades)))
    cowboy))

(define (render-hp hp)
  (for (i 0 hp)
       (t80::spr 272 (* i 9) 0 15 1 0 0 1 1)))

(define (make-bullet x y direction speed)
  (define (update e)
    (set! (e :duration) (- (e :duration) 1))
    (when (<= (e :duration) 0)
      (set! (e :destroy?) #t))
    (let* ((dir-x (car direction))
	   (dir-y (cdr direction)))
      (set! (e :x) (+ (e :x) (* dir-x (/ (e :speed) 10))))
      (set! (e :y) (+ (e :y) (* dir-y (/ (e :speed) 10))))))

  (define (render self)
    (t80::spr 272 (camera-transform (self :x) (self :y)) 15 1 0 0 1 1))

  (hash-table :x x
	      :y y
	      :radius 8
	      :speed speed
	      :type :projectile
	      :duration 120
	      :damage 5
	      :update update
	      :render render))

(define (make-revolver cowboy)
  (define (update self entity-store)
    (set! (self :cooldown) (- (self :cooldown) 1))
    (when (or (not (self :target))
	      ((self :target) :destroy?))
      (let ((target (find (lambda (e)
			    (and (eq? (e :type) :enemy)
				 (within? cowboy e 64)))
			  (entity-store))))
	(set! (self :target) target)))

    (when (and (self :target)
	       (<= (self :cooldown) 0))
      (set! (self :cooldown) (self :max-cooldown))
      (entity-store 'add! (make-bullet (cowboy :x)
				       (cowboy :y)
				       (direction-to cowboy (self :target))
				       40))))

  (define (render self)
    (when (self :target)
      (let ((target (self :target)))
	(t80::spr 272 (camera-transform (target :x) (target :y)) 15 1 0 0 1 1))))

  (hash-table
   :player cowboy
   :cooldown 60
   :max-cooldown 60
   :target #f
   :damage 1
   :update update
   :render render))

(define (make-entity-store)
  (define entities (list))

  (define (add! e)
    (set! entities (cons e entities)))

  (define (remove-destroyed!)
    (set! entities (filter (lambda (e) (not (e :destroy?))) entities)))

  (lambda* (action . args)
    (case action
      ((add!) (add! (car args)))
      ((remove-destroyed!) (remove-destroyed!))
      (else entities))))

(define (game-scene)

  (define entity-store (make-entity-store))

  (define game-map (generate-map 50)) 
  (define get-collisions (collision-system 16))
  (define cowboy (make-cowboy game-map))

  (entity-store 'add! cowboy)
  (for (i 0 5)
       (entity-store 'add! (make-rat cowboy)))

  (define (input)
    (if (t80::btn 3) (set! (cowboy :x) (+ (* (cowboy :speed) 0.05) (cowboy :x))))
    (if (t80::btn 1) (set! (cowboy :y) (+ (* (cowboy :speed) 0.05) (cowboy :y))))
    (if (t80::btn 2) (set! (cowboy :x) (- (cowboy :x) (* (cowboy :speed) 0.05))))
    (if (t80::btn 0) (set! (cowboy :y) (- (cowboy :y) (* (cowboy :speed) 0.05))))
    
    (if (or (t80::btn 0) (t80::btn 1)
	    (t80::btn 2) (t80::btn 3))
	(set! (cowboy :moving?) #t)
	(set! (cowboy :moving?) #f)))

  (define (update)
    (entity-store 'remove-destroyed!)
    (hash-table-set! *camera* :target-x (cowboy :x))
    (hash-table-set! *camera* :target-y (cowboy :y))
    (update-camera!)

    (for-each
     (lambda (e)
       (case (e :type)
	 ((:enemy)
	  ((e :update) e))
	 ((:projectile)
	  ((e :update) e))
	 ))
     (entity-store))

    (for-each
     (lambda (upgrade)
       ((upgrade :update) upgrade entity-store))
     (cowboy :upgrades))

    (define collisions (get-collisions (entity-store)))
    (for-each dispatch-collision! collisions))

  (define (render cowboy)
    (t80::cls 0)
    (map-render game-map *camera*)
    (t80::elli (camera-transform (cowboy :x) (+ 8 (cowboy :y))) 7 3 0)
    (let ((y-offset (+ -2 (* 2 (sin (/ t 3)))))
	  (rotation (* 0.1 (sin (* t 0.1))))
	  (rotation1 0))
      (if (cowboy :moving?)
	  
	  (draw-sprite (camera-transform (cowboy :x) (+ y-offset (cowboy :y))) 2 (* pi rotation) 0 16 1 1 15)
	  (draw-sprite (camera-transform (cowboy :x) (cowboy :y)) 2 0 0 16 1 1 15)))

    (for-each 
     (lambda (e)
       (case (e :type)
	 ((:enemy)
	  (render-entity e))
	 ((:projectile)
	  ((e :render) e))
	 ))

     (entity-store))

    (for-each
     (lambda (upgrade)
       ((upgrade :render) upgrade))
     (cowboy :upgrades))

    (render-hp (cowboy :hp))
    )

  (lambda ()
    (input)
    (update)
    (render cowboy)))

(define t 0)
(define scene (game-scene))

(define (TIC)
  (co/pump-coroutines!)
  (scene)
  
  (inc! t 1))

;; <TILES>
;; 002:6666666666666666666676766666676666666666666666666666666666666666
;; 003:6666666666666666666666666666666666666666666666666666666666666666
;; 004:6666666666666666666666666666666666666666666666666666666666666666
;; 005:6666666666666666666666666666666666666666666666666666666666666666
;; 006:6666666666666666666666666666666666666666666666666666666666666666
;; 007:6666666666666660666666006666600766660000666007706607777060770770
;; 018:6666666666666666666666666676766666676666666666666666666666666666
;; 019:6666666666676766666676666666666666666666666666666666666666666666
;; 020:6666666666666666666666666666666666666666666666666666666666666666
;; 021:6666666666666666666666666666666666666666666666666666666666666666
;; 022:6666666666666060666666006666607766060777666077776607777060007777
;; 023:0777707077707770007077700077777000077077700070077000770777000700
;; 032:fff00fff00f010ff0300110001113000000110ffff0310ffff0110ffff0110ff
;; 034:6666666600000000777077777770777777700777777707777770077777007777
;; 035:6666666600000000770777777077777770777770077777700077777070077770
;; 050:7707777777077777077077770070777770700777707707777007000000000000
;; 051:7700777077707770770077007000700770007007700070070000000700000000
;; 064:fffff111fff11000ff100000f1000011f1001100100010001001000110010010
;; 065:111fffff00011fff000001ff1100001f0011001f000010011100010100100101
;; 066:ffffffff11111fff0100111f1000001100000000000010000000110000000100
;; 067:ffffffffffffffffffffffff1fffffff11ffffff01ffffff011fffff00011111
;; 080:100100101001000110001000f1000100f1000011ff100000fff11000fffff111
;; 081:0001010110010101000100010010001f1100001f000001ff00011fff111fffff
;; 082:11111000000000000011000000111100001ff11111ffffffffffffffffffffff
;; 083:000000110000011f000001ff00011fff1111ffffffffffffffffffffffffffff
;; </TILES>

;; <SPRITES>
;; 000:f066770f06677770f005500fff0550fff077770ff077770ff066660ff060060f
;; 016:f22ff22f222222222242222122422221f222221fff2221fffff21fffffffffff
;; 017:f00ff00f000000000000000000000000f000000fff0000fffff00fffffffffff
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

