;; title:   Cowboy Survivors
;; author:  TofuTheLoafu
;; desc:    Vampire Survivors-like but western themed with tic80
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  scheme

(define-macro (module name . body)
  `(define ,name (with-let (inlet) ,@body (curlet))))

(define (module/prefix module prefix)
  (map (lambda (b)
         (cons (string->symbol (string-append (symbol->string prefix) (symbol->string (car b)))) (cdr b)))
       (let->list module)))

(define-macro (import module)
  `(varlet (curlet) ,module))

(define-macro (import-as module prefix)
  `(apply varlet (curlet) (module/prefix ,module ',prefix)))

(define-macro (define-record-type type-name constructor-spec predicate-name . field-specs)
  (let* ((ctor-name (car constructor-spec))
     (ctor-fields (cdr constructor-spec))
     (fields (map car field-specs)))
    `(begin
       (define (,ctor-name ,@ctor-fields)
     (vector ',type-name
         ,@(map (lambda (f)
              (if (memq f ctor-fields) f #f))
            fields)))

       (define (,predicate-name obj)
     (eq? ',type-name (vector-ref obj 0)))

       ,@(apply append 
        (do ((i 1 (+ i 1))
             (specs field-specs (cdr specs))
             (acc '() (cons
                   (let* ((spec (car specs))
                      (getter (cadr spec))
                      (setter (and (pair? (cddr spec)) (caddr spec))))
                 (cons `(define (,getter obj)
                      (vector-ref obj ,i))
                       (if setter
                       (list `(define (,setter obj val)
                            (vector-set! obj ,i val)))
                       '())))
                   acc)))
            ((null? specs) (reverse acc)))))))

(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))

(define-macro (for args . body)
  (let ((loop (gensym))
	(limit (gensym))
	(binding (car args))
	(from (cadr args))
	(to (caddr args)))
    `(let ((,limit ,to))
       (let ,loop ((,binding ,from))
	    (when (< ,binding ,limit)
	      ,@body
	      (,loop (+ ,binding 1)))))))

(define-constant screen-width 240)
(define-constant screen-height 136)

(module coroutines
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
       to-run))))

(module utility 
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
    (if (null? lst)
	#f
	(let ((len (length lst)))
	  (list-ref lst (random len)))))

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

  (define (merge-hash-table-into! a b f)
    (for-each
     (lambda (entry)
       (let ((key (car entry)))
	 (if (a key)
	     (set! (a key) (f (a key) (b key)))
	     (set! (a key) (b key)))))
     b)
    a)

  (define (ease-out-cubic p) (let ((f (- p 1))) (+ 1 (* f f f))))
  (define (ease-in-cubic p) (* p p p))
  (define (ease-in-out-cubic p)
    (if (< p 0.5)
	(* 4 p p p)
	(+ 1 (/ (* (- (* 2 p) 2) (- (* (- (* 2 p) 2)) (- (* 2 p) 2)) (- (* 2 p) 2)) 2)))))

(module scene-module
  (define *scenes* (list))
  (define (push-scene! scene) (set! *scenes* (cons scene *scenes*))) 
  (define (pop-scene!) (set! *scenes* (cdr *scenes*))) 
  (define (set-scene! scene) (set! *scenes* (list scene))) 
  (define (swap-below! new-scene)
    (set! *scenes* (cons (car *scenes*) (cons new-scene (cddr *scenes*)))))
  (define (current-scene) (car *scenes*))

  (define (run-scenes!)
    (when (pair? *scenes*)
      (let ((top (current-scene)))
	((top :input))
	((top :update))
	(for-each (lambda (s) ((s :render))) (reverse *scenes*))))))

(module camera
  (import utility)
  (define *camera*
    (hash-table :x 0 :y 0 :target-x 10 :target-y 10 :cx 120 :cy 68))

  (define (update-camera!)
    (set! (*camera* :x) (lerp 0.05 (*camera* :x) (*camera* :target-x)))
    (set! (*camera* :y) (lerp 0.05 (*camera* :y) (*camera* :target-y))))

  (define (camera-transform wx wy)
    (values (floor (+ (*camera* :cx) (- wx (*camera* :x))))
	    (floor (+ (*camera* :cy) (- wy (*camera* :y)))))))

(module events 
    (define *events* (list))

  (define (add-event! e)
    (set! *events* (cons e *events*)))

  (define (process-events f)
    (for-each f *events*)
    (set! *events* (list))))


(module world
  (import utility)
  (import camera)
  (define (generate-map width)
    (let ((game-map (make-vector (* width width) 'empty))
	  (center (/ (- width 1) 2.0))
	  (radius (- (/ width 2.0) 2)))
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

  (define (real-coords->world x y) 
    (cons 
     (floor-quotient x 16)
     (floor-quotient y 16))) 

  (define (random-direction)
    (let ((dir (* pi (random 2.0)))) 
      (cons (cos dir) (sin dir)))) 

  (define (map-render map-table camera)
    (let* ((game-map (map-table :map))
	   (map-width (map-table :width))
	   (map-height (map-table :height))
	   (margin 1)
	   (cam-tile-x (- (floor-quotient (round (*camera* :x)) 16) (floor-quotient (round (*camera* :cx)) 16) margin))
	   (cam-tile-y (- (floor-quotient (round (*camera* :y)) 16) (floor-quotient (round (*camera* :cy)) 16) margin))
	   (span-x 18)
	   (span-y 12))

      (define (in-bounds? x y)
	(and (<= 0 x) (< x map-width)
	     (<= 0 y) (< y map-height)))

      (let loop ((i 0))
	(when (< i (* span-x span-y))
	  (let* ((screen-x (modulo i span-x)) 
		 (screen-y (quotient i span-x))
		 (world-x (+ cam-tile-x screen-x)) 
		 (world-y (+ cam-tile-y screen-y)))
	    (when (and (in-bounds? world-x world-y))
	      (let ((sx (floor (+ (*camera* :cx) (- (* 16 world-x) (*camera* :x)))))
		    (sy (floor (+ (*camera* :cy) (- (* 16 world-y) (*camera* :y))))))
		(if (eq? (map-get-cell map-table world-x world-y) 'empty)
		    (t80::rect sx sy 16 16 6)
		    (t80::spr (map-get-cell map-table world-x world-y) sx sy -1 1 0 0 2 2))))
	    (loop (+ i 1)))))

      (let loop ((i 0))
	(when (< i (* span-x span-y))
	  (let* ((screen-x (- (modulo i span-x) 1)) 
		 (screen-y (- (quotient i span-x) 1))
		 (world-x (+ cam-tile-x screen-x)) 
		 (world-y (+ cam-tile-y screen-y)))
	    (when (or (not (in-bounds? world-x world-y))
		      (eq? (map-get-cell map-table world-x world-y) 'empty))
	      (let ((sx (floor (+ (*camera* :cx) (- (* 16 world-x) (*camera* :x)))))
		    (sy (floor (+ (*camera* :cy) (- (* 16 world-y) (*camera* :y))))))
		(for-each
		 (lambda (offset)
		   (t80::spr 64
			     (+ (car offset) sx)
			     (+ (cadr offset) sy)
			     15 1 0 (caddr offset) 2 2))
		 '((-4 -4 0)
		   (4 4 0)))))
	    (loop (+ i 1)))))
      )))

(module geometry
  (define (vec2 x y)
    (cons x y))

  (define (vec2-x v)
    (car v))
  (define (vec2-y v)
    (cdr v))

  (define (vec2-add a b)
    (vec2 (+ (vec2-x a) (vec2-x b))
	  (+ (vec2-y a) (vec2-y b))))

  (define (vec2-sub a b)
    (vec2 (- (vec2-x a) (vec2-x b))
	  (- (vec2-y a) (vec2-y b))))

  (define (vec2-scale a s)
    (vec2 (* (vec2-x a) s)
	  (* (vec2-y a) s)))

  (define (overlapping? a b)
    (let* ((dist-x (- (a :x) (b :x)))
	   (dist-y (- (a :y) (b :y)))
	   (dist-sq (+ (* dist-x dist-x) (* dist-y dist-y)))
	   (min-dist (+ (a :radius) (b :radius))))
      (< dist-sq (* min-dist min-dist))))

  (define (within-raw? x1 y1 x2 y2 dist)
    (let* ((dist-x (- x1 x2))
	   (dist-y (- y1 y2))
	   (dist-sq (+ (* dist-x dist-x) (* dist-y dist-y))))
      (<= dist-sq (* dist dist))))

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
            (cons (/ dx dist) (/ dy dist)))))))

(module collision
  (import geometry)

  (define (remove-entity lst e)
    (cond
     ((null? lst) '())
     ((eq? (car lst) e) (cdr lst))
     (else (cons (car lst) (remove-entity (cdr lst) e)))))

  (define (push-apart! e other)
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
      (set! (other :y) (- (other :y) push-y))))

  (define (make-collision-world cell-size)
    (define grid (hash-table))
    (define members '())
    (define handlers (hash-table))

    (define (cell-key x y)
      (+ (* (floor (/ x cell-size)) 100000) (floor (/ y cell-size))))

    (define (bucket-add! key e)
      (set! (grid key) (cons e (or (grid key) '()))))

    (define (bucket-remove! key e)
      (when (grid key) (set! (grid key) (remove-entity (grid key) e))))

    (define (register! e)
      (unless (memq e members)
	(set! members (cons e members))
	(let ((key (cell-key (e :x) (e :y))))
	  (set! (e :collision-key) key)
	  (bucket-add! key e))))

    (define (unregister! e)
      (set! members (remove-entity members e))
      (let ((key (e :collision-key)))
	(when key (bucket-remove! key e))
	(set! (e :collision-key) #f)))

    (define (update-membership! e)
      (let ((new-key (cell-key (e :x) (e :y)))
	    (old-key (e :collision-key)))
	(when (not (equal? old-key new-key))
	  (when old-key (bucket-remove! old-key e))
	  (bucket-add! new-key e)
	  (set! (e :collision-key) new-key))))

    (define (on-collision! a-type b-type handler)
      (hash-table-set! handlers (cons a-type b-type) handler))
    (define (dispatch-collision! a b)
      (let* ((a-type (a :type))
	     (b-type (b :type)))
	(cond ((handlers (cons a-type b-type)) => (lambda (h) (h a b)))
	      ((handlers (cons b-type a-type)) => (lambda (h) (h b a)))
	      (else #f))))

    (define (check-pair! a b)
      (when (overlapping? a b)
	(if (and (eq? (a :type) :enemy) (eq? (b :type) :enemy))
	    (push-apart! a b)
	    (dispatch-collision! a b))))

    (define neighbor-offsets '((1 . 0) (1 . 1) (0 . 1) (-1 . 1)))

    (define (nearby-entities x y radius)
      (let ((cx (floor (/ x cell-size)))
	    (cy (floor (/ y cell-size)))
	    (cell-radius (max 1 (ceiling (/ radius cell-size))))
	    (buckets '()))
	(let loop-dx ((dx (- cell-radius)))
	  (when (<= dx cell-radius)
	    (let loop-dy ((dy (- cell-radius)))
	      (when (<= dy cell-radius)
		(let ((bucket (grid (cell-key (* cell-size (+ cx dx)) (* cell-size (+ cy dy))))))
		  (when (pair? bucket)
		    (set! buckets (cons bucket buckets))))
		(loop-dy (+ dy 1))))
	    (loop-dx (+ dx 1))))
	(apply append buckets)))

    (define (find-and-dispatch-collisions)
      (for-each
       (lambda (grid-entry)
	 (let* ((key (car grid-entry))
		(bucket (cdr grid-entry))
		(cx (floor (/ key 100000)))
		(cy (- key (* cx 100000))))
	   (let loop ((lst bucket))
	     (when (pair? lst)
	       (let ((a (car lst)))
		 (for-each (lambda (b) (check-pair! a b)) (cdr lst)))
	       (loop (cdr lst))))

	   (for-each
            (lambda (offset)
	      (let ((neighbor-cell (grid (cell-key (* cell-size (+ cx (car offset))) (* cell-size (+ cy (cdr offset)))))))
                (when neighbor-cell
                  (for-each
                   (lambda (a)
                     (for-each
		      (lambda (b)
                        (check-pair! a b))
		      neighbor-cell))
                   bucket))))
            neighbor-offsets)))
       grid))

    (define (resolve!)
      (for-each update-membership! members)
      (find-and-dispatch-collisions))

    (lambda* (action . args)
      (case action
	((register!) (apply register! args))
	((unregister!) (apply unregister! args))
	((on-collision!) (apply on-collision! args))
	((nearby-entities) (apply nearby-entities args))
	((resolve!) (resolve!))
	(else members)))))

(module entity-helpers
  (define (update-hit-debounce e)
    (when (and (e :hit-debounce) )
      (if (< 0 (e :hit-debounce))
	  (set! (e :hit-debounce) (- (e :hit-debounce) 1)) 
	  (begin 
	    (set! (e :hit-debounce) #f)
	    (set! (e :white-flash?) #f)))))

  (define (update-duration e)
      (when (e :duration)
	(set! (e :duration) (- (e :duration) 1))
	(when (<= (e :duration) 0)
	  (set! (e :destroy?) #t))))

  (define (update-time e)
    (when (e :time)
      (set! (e :time) (+ (e :time) 1))))

  (define (update-direction e)
    (when (e :direction)
      (let* ((dir-x (car (e :direction)))
	     (dir-y (cdr (e :direction))))
	(set! (e :x) (+ (e :x) (* dir-x (/ (e :speed) 10))))
	(set! (e :y) (+ (e :y) (* dir-y (/ (e :speed) 10))))))))

(module enemies 
  (import collision)
  (import entity-helpers)
       
  (define (enemy-follow-player e)
    (let* ((dir (direction-to e (e :target)))
	   (dir-x (car dir))
	   (dir-y (cdr dir)))
      (set! (e :x) (+ (e :x) (* dir-x (/ (e :speed) 10))))
      (set! (e :y) (+ (e :y) (* dir-y (/ (e :speed) 10))))))

  (define (sprite-frame e)
    (let ((sprite (e :sprite)))
      (if (pair? sprite)
	  (list-ref sprite (modulo (quotient (e :time) 10) (length sprite)))
	  sprite)))

  (define (draw-sprite-frame e frame)
    (if (pair? frame)
	(t80::spr (car frame) (camera-transform (e :x) (e :y)) 15 (or (e :scale) 1) 0 0 (cadr frame) (caddr frame))
	(t80::spr frame (camera-transform (e :x) (e :y)) 15 (or (e :scale) 1) 0 0 1 1)))

  (define (render-entity e)
    (if (e :white-flash?)
	(begin 
	  (for (c 0 15) (t80::poke4 (+ (* #x3ff0 2) c) 4))
	  (draw-sprite-frame e (sprite-frame e))
	  (for (c 0 15) (t80::poke4 (+ (* #x3ff0 2) c) c)))
	(draw-sprite-frame e (sprite-frame e))))
  
  (define (make-tumbleweed x y target)
    (hash-table
     :type :enemy
     :x x 
     :y y 
     :radius 4
     :speed 2
     :time 0
     :target target
     :update enemy-follow-player
     :sprite '(304 305)
     :drop-xp 1
     :hp 10))

  (define (make-big-tumbleweed x y target)
    (hash-table
     :type :enemy
     :x x 
     :y y 
     :radius 8
     :speed 1
     :time 0
     :target target
     :update enemy-follow-player
     :scale 2
     :sprite '(304 305)
     :drop-xp 1
     :hp 20))

  (define (scarab-queen-update e)
    (if (not (within? (e :target) e 72))
      (let* ((dir (direction-to e (e :target)))
	     (dir-x (car dir))
	     (dir-y (cdr dir)))
	(set! (e :x) (+ (e :x) (* dir-x (/ (e :speed) 10))))
	(set! (e :y) (+ (e :y) (* dir-y (/ (e :speed) 10)))))
      (when (= 0 (modulo (e :time) 240))
	(add-event! (vector 'spawn-scarab (e :x) (e :y) (e :target)))
	(add-event! (vector 'spawn-scarab (e :x) (e :y) (e :target)))
	(add-event! (vector 'spawn-scarab (e :x) (e :y) (e :target))))))

  (define (make-scarab-queen x y target)
    (hash-table
     :type :enemy
     :x x 
     :y y 
     :radius 8
     :speed 2
     :time 0
     :target target
     :update scarab-queen-update
     :scale 1
     :sprite '((320 2 2) (322 2 2))
     :drop-xp 4
     :hp 15))

  (define (make-scarab x y target)
    (hash-table
     :type :enemy
     :x x 
     :y y 
     :radius 4
     :speed 4
     :time 0
     :target target
     :update enemy-follow-player
     :scale 1
     :sprite 306 
     :hp 1))

  (define (render-snake snake)
    (let loop ((links (snake :links)))
      (when (pair? links)
	(let ((link (car links)))
	  (if (link :white-flash?)
	      (begin 
		(for (c 0 15) (t80::poke4 (+ (* #x3ff0 2) c) 4))
		(draw-sprite (camera-transform (link :x) (link :y)) 1 (- (link :angle)) (modulo (link :sprite) 16) (quotient (link :sprite) 16) 1 1 15)
		(for (c 0 15) (t80::poke4 (+ (* #x3ff0 2) c) c)))
	      (draw-sprite (camera-transform (link :x) (link :y)) 1 (- (link :angle)) (modulo (link :sprite) 16) (quotient (link :sprite) 16) 1 1 15))
	  (loop (cdr links))))))

  (define (update-snake snake)
    (let* ((head (car (snake :links)))
	   (target-angle (atan (- (snake :target :y) (head :y))
			       (- (snake :target :x) (head :x))))
	   (angle-diff (- (modulo (+ (- target-angle (head :angle)) pi)
				  (* 2 pi)) pi))
	   (max-turn 0.010)
	   (turn-factor (/ (abs angle-diff) pi))
	   (speed (* 1 (- 1.0 (* 0.6 turn-factor)))))

      (set! (head :angle)
	(+ (head :angle)
	   (max (- max-turn) (min max-turn angle-diff))))

      (set! (head :x)
	(+ (head :x) (* speed (cos (head :angle)))))
      
      (set! (head :y)
	(+ (head :y) (* speed (sin (head :angle)))))

      (when (null? (cdr (snake :links)))
	(set! (snake :destroy?) #t))

      (let loop ((links (cdr (snake :links)))
		 (prev-link head))
	(when (pair? links)
	  (let* ((link (car links))
		 (dx (- (prev-link :x) (link :x)))
		 (dy (- (prev-link :y) (link :y)))
		 (dist (sqrt (+ (* dx dx) (* dy dy)))))
	    (update-hit-debounce link)
	    (when (<= (link :hp) 0)
	      (add-event! (vector 'collision-unregister link))
	      (set! (link :destroy?) #t))
	    (set! (link :angle) (prev-link :angle))
	    (when (< 8 dist)
	      (set! (link :x) (+ (link :x) (* (/ dx dist)
					      (- dist 8))))
	      (set! (link :y) (+ (link :y) (* (/ dy dist)
					      (- dist 8)))))
	    (loop (cdr links) link))))

      (set! (snake :links) (filter (lambda (l) (not (l :destroy?))) (snake :links)))
      ))

  (define (make-snake-link tail?)
    (hash-table
     :type :link
     :enemy-type :snake-link
     :x 0
     :y 0
     :radius 8
     :angle 0
     :sprite (if tail? 310 309)
     :hp 20))

  (define (make-snake-head x y)
    (hash-table
     :type :link-head
     :x x
     :y y
     :speed 0
     :radius 8
     :angle 0
     :sprite 308))

  (define (make-snake x y target)
    (define links (list (make-snake-link #t)))
    (for (i 0 20)
	 (set! links (cons (make-snake-link #f) links)))
    (hash-table
     :type :boss
     :update update-snake
     :render render-snake
     :target target
     :links (cons (make-snake-head x y) links)))
  

  (define (damage-enemy! e amount)
    (if (e :hit-debounce)
	#f
	(begin 
	  (set! (e :hp) (- (e :hp) amount))
	  (set! (e :hit-debounce) 8)
	  (set! (e :white-flash?) #t)))))

(module gem-module
  (import geometry)
  (define (make-xp-orb x y)
    (vector x y 259))

  (define (gem-x gem)
    (vector-ref gem 0))
   
  (define (gem-y gem)
    (vector-ref gem 1))

  (define (gem-sprite gem)
    (vector-ref gem 2))

  (define (render-gem gem)
    (t80::spr (gem-sprite gem) (camera-transform (gem-x gem) (gem-y gem)) 15 1 0 0 1 1))

  (define (update-gem gem cowboy collect-xp!)
    (if (within-raw? (gem-x gem) (gem-y gem) (cowboy :x) (cowboy :y) 16)
	(begin (collect-xp! cowboy 1)
	       #f)
	gem)))

(module ui-module 
  (import-as coroutines co/)

  (define (render-time game-time)
    (let ((seconds (quotient game-time 60)))
      (t80::print (string-append "Time " (number->string seconds)) 195 2 0)
      (t80::print (string-append "Time " (number->string seconds)) 196 2 4)))

  (define (render-hp hp)
    (for (i 0 hp)
      (t80::spr 272 (* i 9) 0 15 1 0 0 1 1)))

  (define (render-xp xp xp-needed)
    (t80::rect 0 (- screen-height 4) (floor (* screen-width (/ xp xp-needed))) 4 13))

  (define* (shake-effect! intensity (duration 20))
    (co/spawn! 
     (lambda (yield)
       (do ((i 0 (+ i 1))) ((= i duration))
	 (t80::poke #x3FF9 (- (random (+ 1 (* 2 intensity))) intensity))
	 (t80::poke (+ #x3FF9 1) (- (random (+ 1 (* 2 intensity))) intensity))
	 (yield))
       (t80::memset #x3FF9 0 2))))

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
	       (sv 3 2) (sv 3 3) 0 key)))


(module projectile
  (define (update-bullet e) #t)
  
  (define (render-bullet self)
    (t80::spr (self :sprite) (camera-transform (self :x) (self :y)) 15 1 0 0 1 1))

  (define (make-projectile x y direction speed damage)
    (hash-table :x x
		:y y
		:radius 6
		:speed speed
		:type :projectile
		:direction direction
		:sprite 258
		:duration 120
		:damage damage
		:update update-bullet
		:render render-bullet)))
(module damage-item 
  (define (damage-blueprint)
    (hash-table
     :id :damage-item
     :type :upgrade
     :sprite 262
     :name "Damage"
     :damage 1
     :stats '(:damage)
     :description "Gives extra damage"))

  (define (make-damage-item cowboy)
    (let ((new-damage (damage-blueprint)))
      (set! (cowboy :extra-damage) (+ 1 (cowboy :extra-damage)))
      new-damage)))

(module speed-item 
  (define (speed-blueprint)
    (hash-table
     :id :speed-item
     :type :upgrade
     :sprite 261
     :name "Speed"
     :description "Gives extra speed"))

  (define (make-speed-item cowboy)
    (let ((new-speed (speed-blueprint)))
      (set! (cowboy :speed) (+ 2 (cowboy :speed)))
      new-speed)))

(module extra-heart 
  (define (heart-blueprint)
      (hash-table
       :id :heart
       :type :upgrade
       :sprite 272
       :name "Heart"
       :description "Gives 1 extra life"))

  (define (make-extra-heart cowboy)
    (let ((new-heart (heart-blueprint)))
      (set! (cowboy :hp) (+ 1 (cowboy :hp)))
      new-heart)))

(module sword 
  (import utility)
  (import geometry)
  (define (sword-position cowboy t index)
    (let* ((sword-upgrades (filter (lambda (u) (eq? (u :id) :sword)) (cowboy :upgrades)))
	   (sword-count (cowboy :sword-count))
	   (angle (+ (* t 0.05) (* index (/ (* 2 pi) sword-count))))
	   (dir-vector (vec2 (cos angle) (sin angle)))
	   (cowboy-vec (vec2 (cowboy :x) (cowboy :y)))
	   (new-position (vec2-scale dir-vector 32))) 
      (vec2-add new-position cowboy-vec)))

  (define (update-sword sword game-time)
    (define position (sword-position (sword :cowboy) game-time (sword :index)))
    (set! (sword :x) (floor (vec2-x position)))
    (set! (sword :y) (floor (vec2-y position))))

  (define (render-sword self)
    (t80::spr (self :sprite) (camera-transform (self :x) (self :y)) 15 1 0 0 1 1))
  
  (define (make-sword cowboy index)
    (hash-table :x (cowboy :x)
		:y (cowboy :y)
		:cowboy cowboy
		:radius 8
		:type :weapon
		:sprite 263
		:index index
		:damage 1
		:update update-sword
		:render render-sword))

  (define (update-sword-blueprint self entity-store collision-world)
    (when (not (self :sword))
      (let* ((sword-count (length (filter (lambda (u) (eq? (u :id) :sword)) (self :player :upgrades))))
	     (new-sword (make-sword (self :player) sword-count)))
	(set! (self :player :sword-count) sword-count)
	(set! (self :sword) new-sword)
	(entity-store 'add! new-sword)
	(collision-world 'register! new-sword))))

  (define (sword-blueprint)
      (hash-table
       :id :sword
       :type :upgrade
       :name "Sword"
       :sprite 263
       :range 32 
       :sword #f
       :damage 1 
       :stats '(:damage)
       :update update-sword-blueprint))

  (define (make-sword-gear cowboy)
    (let ((new-sword-blueprint (sword-blueprint)))
      (set! (new-sword-blueprint :player) cowboy)
      new-sword-blueprint)))

(module revolver 
  (import projectile)
  (import geometry)

  (define (make-bullet x y direction speed damage)
    (make-projectile x y direction speed damage))

  (define (render-revolver self)
    (when (self :target)
	(let ((target (self :target)))
	  (t80::spr 257 (camera-transform (- (target :x) 3) (- (target :y) 3)) 15 2 0 0 1 1))))
  
  (define (update-revolver self entity-store collision-world)
    (let ((cowboy (self :player)))
      (when (and (self :target) (self :target :destroy?))
	(set! (self :target) #f))
      (if (self :target)
	  (set! (self :cooldown) (- (self :cooldown) 1 ))
	  (let ((target (list->random (filter (lambda (e)
						(and (or (eq? (e :type) :enemy) (eq? (e :type) :link))
						     (within? cowboy e (self :range))))
					      (collision-world 'nearby-entities (cowboy :x) (cowboy :y) (self :range))))))
	    (set! (self :target) target)))
      
      (when (and (self :target)  (<= (self :cooldown) 0))
	(let ((bullet (make-bullet (cowboy :x) (cowboy :y)
				   (direction-to cowboy (self :target))
				   40
				   (+ (cowboy :extra-damage) (self :damage)) )))
	  (entity-store 'add! bullet)
	  (collision-world 'register! bullet))
	(set! (self :target) #f)
	(set! (self :cooldown) (self :max-cooldown)))))

  (define (revolver-blueprint)
      (hash-table
       :id :revolver
       :type :upgrade
       :sprite 260
       :name "Revolver"
       :description "Targets 1 enemy at a time, fires a bullet"
       :cooldown 120 
       :max-cooldown 120 
       :range 64 
       :target #f
       :damage 7 
       :stats '(:max-cooldown :range :damage)
       :update update-revolver
       :render render-revolver))

  (define (make-revolver cowboy)
    (let ((new-revolver (revolver-blueprint)))
      (set! (new-revolver :player) cowboy)
      new-revolver)))

(module entity-store 
  (import utility)
  (import events)
  (import-as entity-helpers eh/)  
  
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

  (define (update-entity e game-time)
    (eh/update-duration e)
    (eh/update-direction e)
    (eh/update-time e)
    (eh/update-hit-debounce e)
    
    (case (e :type)
      ((:enemy)
       (when (and (e :hp) (<= (e :hp) 0))
	 (when (e :drop-xp)
	   (add-event! (vector 'spawn-xp (e :x) (e :y) (e :drop-xp))))
	 (set! (e :destroy?) #t)
	 (add-event! (vector 'collision-unregister e)))
       (when (e :update)
	 ((e :update) e)))

      ((:projectile :boss)
       ((e :update) e))

      ((:weapon)
       ((e :update) e game-time)))))

(module level-up-scene
  (import scene-module)
  (import revolver)
  (import extra-heart)
  (import speed-item)
  (import damage-item)
  (import sword)

  (define *gear-catalog*
    (hash-table
     :revolver (cons (revolver-blueprint) make-revolver)
     :heart (cons (heart-blueprint) make-extra-heart)
     :speed-item (cons (speed-blueprint) make-speed-item)
     :damage-item (cons (damage-blueprint) make-damage-item)
     :sword (cons (sword-blueprint) make-sword-gear))) 

  (define (randomize-list lst)
    (map cdr
	 (sort! (map (lambda (x) (cons (random (length lst)) x)) lst)
		(lambda (a b) (< (car a) (car b)))))) 

  (define (choose-upgrade-options)
    (define gear-catalog-list (randomize-list (map values *gear-catalog*))) 
    (list 
     (cdr (car gear-catalog-list))
     (cdr (cadr gear-catalog-list))
     (cdr (caddr gear-catalog-list))))

  (define (apply-gear! cowboy upgrade)
    (let* ((new-upgrade ((cdr upgrade) cowboy)))
      (set! (cowboy :upgrades) (cons new-upgrade (cowboy :upgrades)))
      cowboy))

  (define (render-level-up-menu selected options)
    (t80::rect 13 13 (- screen-width 26) (- screen-height 26) 0)
    (t80::rect 15 15 (- screen-width 30) (- screen-height 30) 7)
    (t80::print "Choose an upgrade" 17 17 0)
    (t80::print "Choose an upgrade" 18 18 3)
    (for-each
     (lambda (option i)
       (let* ((card-height (- screen-height 60))
	      (card-width (- (/ screen-width 3) 20))
	      (card-start-y 30)
	      (card-start-x (+ 25 (* i (+ 5 card-width))))
	      (option (car option)))
	 (when (= i selected)
	   (t80::rect (- card-start-x 3) (- card-start-y 3) (+ card-width 6) (+ card-height 6) 8))
	 (t80::rect card-start-x card-start-y card-width card-height 3)
	 (t80::spr (option :sprite) (+ 2 card-start-x) (+ 2 card-start-y) 15 2 0 0 1 1)
	 (t80::print (option :name) (+ 2 card-start-x) (+ 20 card-start-y) 0)
	 (when (option :stats)
	   (for-each
	    (lambda (stat j)
	      (let ((display-name (case stat
				    ((:max-cooldown) "cooldown")
				    (else (symbol->string (keyword->symbol stat))))))
		(t80::print display-name (+ 2 card-start-x) (+ 30 (* j 10) card-start-y) 0 #t 1 #t)
		(t80::print (object->string (option stat)) (+ 42 card-start-x) (+ 30 (* j 10) card-start-y) 0)))
	    (option :stats)
	    (range (length (option :stats)))))))
     options
     (range (length options))))

  (define (make-level-up-scene cowboy)
    (define options (choose-upgrade-options))
    (define selected 0)

    (hash-table
     :input (lambda ()
		  (when (t80::btnp 3) (set! selected (modulo (+ selected 1) (length options))))
		  (when (t80::btnp 2) (set! selected (modulo (- selected 1) (length options))))
		  (when (t80::btnp 4)
		    (apply-gear! cowboy (list-ref options selected))
		    (pop-scene!)))
     :update (lambda () #t)
     :render (lambda () (render-level-up-menu selected options)))))

(module cowboy-module
  (import camera)
  (import scene-module)
  (import level-up-scene)
  
  (define-constant cowboy-sprite 256)

  (define (damage-cowboy! e amount)
      (when (not (e :hit-debounce))
	(set! (e :hp) (- (e :hp) amount))
	(shake-effect! 2 10)
	(set! (e :hit-debounce) 50)
	(set! (e :white-flash?) #t)
	(co/spawn!
	 (lambda (yield) 
	   (for (i 0 6) (yield))
	   (set! (e :white-flash?) #f)))))

  (define (render-cowboy cowboy)
    (t80::elli (camera-transform (cowboy :x) (+ 8 (cowboy :y))) 7 3 0)
    (let ((y-offset (+ -2 (* 2 (sin (/ t 3)))))
	  (rotation (* 0.1 (sin (* t 0.1))))
	  (rotation1 0))
      (when (cowboy :white-flash?)
	(for (c 0 15) (t80::poke4 (+ (* #x3ff0 2) c) 4)))

      (if (cowboy :moving?)
	  (draw-sprite (camera-transform (cowboy :x) (+ y-offset (cowboy :y))) 2 (* pi rotation) 0 16 1 1 15)
	  (draw-sprite (camera-transform (cowboy :x) (cowboy :y)) 2 0 0 16 1 1 15))

      (when (cowboy :white-flash?)
	(for (c 0 15) (t80::poke4 (+ (* #x3ff0 2) c) c)))))

  (define (xp-needed level)
    (+ 2 (* level 3)))

  (define (level-up! cowboy)
    (set! (cowboy :level) (+ 1 (cowboy :level)))
    (set! (cowboy :xp) 0)
    (push-scene! (make-level-up-scene cowboy)))

  (define (collect-xp! cowboy amount)
    (set! (cowboy :xp) (+ amount (cowboy :xp)))
    (when (<= (xp-needed (cowboy :level)) (cowboy :xp))
      (level-up! cowboy)))

  (define (move-cowboy-x! map cowboy pos?)
    (let* ((new-x ((if pos? + -) (cowboy :x) (* (cowboy :speed) 0.05)))
	   (world-coords (real-coords->world (round new-x) (round (cowboy :y))))
	   (tile (map-get-cell map (car world-coords) (cdr world-coords))))
      (when (not (eq? tile 'empty))
	(set! (cowboy :x) new-x))))

  (define (move-cowboy-y! map cowboy pos?)
    (let* ((new-y ((if pos? + -) (cowboy :y) (* (cowboy :speed) 0.05)))
	   (world-coords (real-coords->world (round (cowboy :x)) (round new-y)))
	   (tile (map-get-cell map (car world-coords) (cdr world-coords))))
      (when (not (eq? tile 'empty))
	(set! (cowboy :y) new-y))))
  
  (define (make-cowboy world-x world-y)
    (let* ((coords (world->real-coords world-x world-y))
	   (cowboy  
	    (hash-table
	     :type :player
	     :x (car coords)
	     :y (cdr coords)
	     :radius 6
	     :speed 10
	     :level 1
	     :xp 0
	     :extra-damage 0
	     :upgrades (list)
	     :update (lambda (x y) #f)
	     :hp 5
	     :moving? #f)))
      cowboy)))

(module collision-rules
  (import enemies)
  (import cowboy-module)

  (define (register-collision-rules! collision-world)
    (collision-world 'on-collision! :projectile :link
		     (lambda (a b)
		       (when (damage-enemy! b (a :damage))
			 (set! (a :destroy?) #t)
			 (add-event! (vector 'collision-unregister a)))))

    (collision-world 'on-collision! :weapon :link
      (lambda (w e) (damage-enemy! e (+ (w :cowboy :extra-damage) (w :damage)))))

    (collision-world 'on-collision! :projectile :enemy
		     (lambda (a b)
		       (when (damage-enemy! b (a :damage))
			 (set! (a :destroy?) #t)
			 (add-event! (vector 'collision-unregister a)))))

    (collision-world 'on-collision! :weapon :enemy
      (lambda (w e) (damage-enemy! e (+ (w :cowboy :extra-damage) (w :damage)))))

    (collision-world 'on-collision! :enemy :enemy push-apart!)

    (collision-world 'on-collision! :player :enemy
      (lambda (a b) (damage-cowboy! a 1)))))

(module enemy-waves
  (import world)
  (import enemies) 

  (define enemy-types
    (list
     (hash-table :id :tumbleweed :cost 1 :min-time 0 :make make-tumbleweed)
     (hash-table :id :big-tumbleweed :cost 2 :min-time 40 :make make-big-tumbleweed)
     (hash-table :id :scarab-queen :cost 7 :min-time 80 :make make-scarab-queen))) 

  (define (available-enemies elapsed)
    (filter (lambda (e) (<= (e :min-time) elapsed)) enemy-types)) 

  (define (current-budget elapsed-seconds)
    (+ 1 (* 0.05 elapsed-seconds))) 

  (define (spend-budget budget elapsed)
    (let ((pool (available-enemies elapsed)))
      (let loop ((remaining budget)
		 (picks '()))
	(let ((affordable (filter (lambda (e) (<= (e :cost) remaining)) pool)))
	  (if (null? affordable)
	      picks
	      (let ((choice (list->random affordable)))
		(loop (- remaining (choice :cost)) (cons choice picks)))))))) 

  (define (get-random-position-radius-cowboy cowboy radius)
    (let* ((dir (random-direction))
 	   (new-x (+ (cowboy :x) (* radius (car dir))))
 	   (new-y (+ (cowboy :y) (* radius (cdr dir)))))
      (cons new-x new-y))) 

  (define (spawn-enemies entity-store collision-world cowboy t)
    (let ((seconds (/ t 60)))
      (when (= 0 (modulo seconds 5))
	(for-each (lambda (enemy)
		    (let* ((coords (get-random-position-radius-cowboy cowboy 128))
			   (entity ((enemy :make) (car coords) (cdr coords) cowboy)))
		      (entity-store 'add! entity)
		      (when (entity :radius)
			(collision-world 'register! entity))))
		  (spend-budget (current-budget seconds) seconds))))))

(module pause-menu
  (import scene-module)

  (define (render-upgrade upgrade selected x y)
    (let ((upgrade-width 16)
	  (upgrade-height 16))
      (if selected
	  (t80::rect (- x 2) (- y 2) (+ 4 upgrade-width) (+ 4 upgrade-height) 14)
	  (t80::rect (- x 1) (- y 1) (+ 2 upgrade-width) (+ 2 upgrade-height) 0))
      (t80::rect x y upgrade-width upgrade-height 4)
      (t80::spr (upgrade :sprite) x y 15 2 0 0 1 1)))

  (define (render-upgrade-list start-x start-y upgrades selected)
    (for-each
     (lambda (upgrade i)
       (let* ((row-width 6)
	      (tile-y (quotient i row-width))
	      (tile-x (modulo i row-width))
	      (x (+ start-x (* tile-x 19)))
	      (y (+ start-y (* tile-y 19)))
	      (selected? (= i selected)))
	 (render-upgrade upgrade selected? x y)))
     upgrades
     (range (length upgrades))))

  (define (render-upgrade-info start-x start-y upgrade)
    (t80::rect start-x 15 (- screen-width start-x 17) (- screen-height 30) 0) 
    (t80::rect (+ 2 start-x) 15 (- screen-width start-x 17) (- screen-height 30) 4) 
    (t80::spr (upgrade :sprite) (+ 2 start-x) (+ 2 15) 15 2 0 0 1 1)
    (t80::print (upgrade :name) (+ 2 start-x) (+ 20 15) 0)
    (when (upgrade :stats)
      (for-each
       (lambda (stat j)
	 (let ((display-name (case stat
			       ((:max-cooldown) "cooldown")
			       (else (symbol->string (keyword->symbol stat))))))
	   (t80::print display-name (+ 2 start-x) (+ 30 (* j 10) start-y) 0 #t 1 #t)
	   (t80::print (object->string (upgrade stat)) (+ 42 start-x) (+ 30 (* j 10) start-y) 0)))
       (upgrade :stats)
       (range (length (upgrade :stats))))))

  (define (render-pause-menu upgrades selected)
    (t80::rect 13 13 (- screen-width 26) (- screen-height 26) 0)
    (t80::rect 15 15 (- screen-width 30) (- screen-height 30) 7)
    (t80::print "Pause" 17 17 0)
    (t80::print "Pause" 18 18 3)
    (render-upgrade-info 130 15 (list-ref upgrades selected))
    (render-upgrade-list 17 26 upgrades selected))

  (define (make-pause-menu upgrades)
    (define selected 0)
    (define upgrades upgrades)
    
    (hash-table
     :input (lambda ()
	      (when (t80::btnp 3) (set! selected (modulo (+ selected 1) (length upgrades))))
	      (when (t80::btnp 2) (set! selected (modulo (- selected 1) (length upgrades))))
	      (when (t80::btnp 5)
		(pop-scene!)))
     :update (lambda () #t)
     :render (lambda () (render-pause-menu upgrades selected)))))

(module game-over-scene
  (import scene-module)

  (define (render-game-over-scene)
    (t80::print "Game Over" 30 30 2)
    (t80::print "Game Over" 31 31 0)
    (t80::print "Press X to continue" 31 61 0))

  (define (make-game-over-scene on-restart)
    (hash-table
     :input (lambda ()
	      (when (t80::btnp 5)
		(on-restart)))
     :update (lambda () #t)
     :render (lambda () (render-game-over-scene)))))

(module game-scene-module 
  (import collision)  
  (import collision-rules)
  (import entity-store)
  (import cowboy-module)
  (import level-up-scene)
  (import ui-module)
  (import enemies)
  (import enemy-waves)
  (import world)
  (import game-over-scene)
  (import pause-menu)
  (import gem-module)

  (define game-time 0)

  (define (make-game-scene)
    (set! game-time 0)
    (define gems (list))
    (define game-map (generate-map 35)) 
    (define entity-store (make-entity-store))
    (define collision-world (make-collision-world 16))
    (register-collision-rules! collision-world)
    (define cowboy (make-cowboy (quotient (game-map :width) 2) (quotient (game-map :height) 2)))
    (collision-world 'register! cowboy)
    (entity-store 'add! cowboy)

    (when (< 120 (/ game-time 60))
      (define snake (make-snake 25 25 cowboy))
      (entity-store 'add! snake)

      (for-each
       (lambda (l)
	 (collision-world 'register! l))
       (cdr (snake :links))))

    (define (update-gem' gem)
      (update-gem gem cowboy collect-xp!))

    (define (update-gems)
      (set! gems (filter update-gem' gems)))

    (set! (*camera* :x) (cowboy :x))
    (set! (*camera* :y) (cowboy :y))

    (define (input)
      (if (t80::btn 3) (move-cowboy-x! game-map cowboy #t))
      (if (t80::btn 1) (move-cowboy-y! game-map cowboy #t))
      (if (t80::btn 2) (move-cowboy-x! game-map cowboy #f))
      (if (t80::btn 0) (move-cowboy-y! game-map cowboy #f))

      (when (t80::btnp 5) (push-scene! (make-pause-menu (cowboy :upgrades))))
      
      (if (or (t80::btn 0) (t80::btn 1)
	      (t80::btn 2) (t80::btn 3))
	  (set! (cowboy :moving?) #t)
	  (set! (cowboy :moving?) #f)))

    (define (update)
      (when (< game-time 3)
	(level-up! cowboy))
      (entity-store 'remove-destroyed!)
      (hash-table-set! *camera* :target-x (cowboy :x))
      (hash-table-set! *camera* :target-y (cowboy :y))
      (update-camera!)

      (set! game-time (+ 1 game-time))
      (when (<= (cowboy :hp) 0)
	(push-scene! (make-game-over-scene
		      (lambda ()
			(push-scene!
			 (make-transition-scene
			  (lambda ()
			    (swap-below! (make-main-menu-scene)))
			  40))))))
      (spawn-enemies entity-store collision-world cowboy game-time)
      (for-each (lambda (e) (update-entity e game-time)) (entity-store))

      (for-each
       (lambda (upgrade)
	 (when (upgrade :update)
	   ((upgrade :update) upgrade entity-store collision-world)))
       (cowboy :upgrades))

      (collision-world 'resolve!)

      (update-gems)

      (process-events
       (lambda (event)
	 (case (event 0)
	   ((collision-unregister) (collision-world 'unregister! (event 1)))
	   ((spawn-xp) (set! gems (cons (make-xp-orb (event 1) (event 2)) gems)))
	   ((spawn-scarab) (entity-store 'add! (make-scarab (event 1) (event 2) (event 3))))))))

    (define (render)
      (t80::cls 0)
      (map-render game-map *camera*)
      (render-cowboy cowboy)

      (for-each 
       (lambda (e)
	 (case (e :type)
	   ((:enemy)
	    (render-entity e))
	   ((:projectile :weapon :boss)
	    ((e :render) e))))
       (entity-store))

      (for-each render-gem gems)

      (for-each
       (lambda (upgrade)
	 (when (upgrade :render)
	   ((upgrade :render) upgrade)))
       (cowboy :upgrades))
     

      (render-hp (cowboy :hp))
      (render-time game-time)
      (render-xp (cowboy :xp) (xp-needed (cowboy :level))))

    (hash-table
     :input input
     :update update
     :render render)))

(module transition-scene
  (import scene-module)
  (import utility)

  (define (render-transition-screen x y)
    (for (i 0 15)
      (for (j 0 9)
	 (t80::spr 66 (+ x (* 16 i)) (+ y (* 16 j)) -1 1 0 0 2 2))))
  
  (define (make-transition-scene on-swap duration)
    (define elapsed 0)
    (define phase :out)
    
    (hash-table
     :input (lambda () #t)
     :update (lambda ()
	       (set! elapsed (+ 1 elapsed))
	       (when (<= duration elapsed)
		 (case phase
		   ((:out)
		    (on-swap)
		    (set! phase :in)
		    (set! elapsed 0))
		   ((:in)
		    (pop-scene!)))))
     :render (lambda ()
	       (let ((progress (/ elapsed duration)))
		 (if (eq? phase :in)
		     (render-transition-screen 0 (floor (* screen-height (ease-out-cubic progress))))
		     (render-transition-screen 0 (floor (- screen-height (* screen-height (ease-out-cubic progress)))))))))))

(module main-menu-scene
  (import scene-module)
  (import game-scene-module)
  (import transition-scene)

  (define (menu-option x y name selected?)
    (t80::print name x y 0)
    (t80::print name (+ 1 x) (+ 1 y) 4)
    (when selected? 
      (t80::tri x (+ 4 y) (- x 4) y (- x 4) (+ 8 y) 14)))
  
  (define (render-main-menu-scene menu-options selected)
    (t80::cls 0)
    (t80::print "Cowboy Survivors v0.9" 5 5 0)
    (t80::print "Cowboy Survivors v0.9"  (+ 1 5) (+ 1 5) 4)
    (for-each 
     (lambda (option i)
       (menu-option 30 30 (option :text) (= selected i)))
     menu-options
     (range (length menu-options))))

  (define (make-main-menu-scene)
    (define selected 0)
    (define menu-options
      (list (hash-table :text "Play"
			:fn (lambda ()
			      (push-scene!
			       (make-transition-scene
				(lambda ()
				  (swap-below! (make-game-scene)))
				40))))))
    
    (hash-table
     :input (lambda ()
	      (when (t80::btnp 1) (set! selected (modulo (+ selected 1) 1)))
	      (when (t80::btnp 4) (set! selected (modulo (- selected 1) 1)))
	      (when (t80::btnp 5)
		(((list-ref menu-options selected) :fn))))
     :update (lambda () #t)
     :render (lambda () (render-main-menu-scene menu-options selected)))))

(import scene-module)
(import-as coroutines co/)
(import main-menu-scene)
(push-scene! (make-main-menu-scene))
(define t 0)

(define (TIC)
  (co/pump-coroutines!)
  (run-scenes!)
  
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
;; 066:0001011110011000001000000100001111001100100010001001000110010010
;; 067:1110001000011001000001001100001000110011000010011100010100100101
;; 080:100100101001000110001000f1000100f1000011ff100000fff11000fffff111
;; 081:0001010110010101000100010010001f1100001f000001ff00011fff111fffff
;; 082:1001001010010001100010001100010001000011001000001101100000100111
;; 083:0001011110011000001000000100001111001100100010001001000110010010
;; </TILES>

;; <SPRITES>
;; 000:f066770f06677770f005500fff0550fff077770ff077770ff066660ff060060f
;; 001:ffffffffff888ffff8f8f8ff8ff8ff8f8888888f8ff8ff8ff8f8f8ffff888fff
;; 002:ffffffffff1111fff122221ff122221ff122221ff122221fff1111ffffffffff
;; 003:fffeefffffeddefffed4ddefffeddefffeddddefffeddefffffeefffffffffff
;; 004:f222ffff321121121211211172223fff77733fff777fffff77ffffff77ffffff
;; 005:8fff8fff88ff88fff88ff88fff88ff88ff88ff88f88ff88f88ff88ff8fff8fff
;; 006:ffffff99fffff999ffff999f9ff999ff66999ffff669ffff6666ffff66f69fff
;; 007:fff43ffffff43ffffff43ffffff43ffffff44ffff888888ffff67ffffff67fff
;; 016:f99ff99f999999999949999a9949999af99999afff999afffff9afffffffffff
;; 017:f00ff00f000000000000000000000000f000000fff0000fffff00fffffffffff
;; 036:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 037:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 038:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 048:fff775fff777757ff755f7577777177717f11f7751551f7f555f11fff777777f
;; 049:f777777fff11f555f7f1551577f11f7177717777757f557ff757777fff577fff
;; 050:ffffffffff3333fff377773f3077773f3077773ff377773fff3333ffffffffff
;; 052:ffffffff111999ff1144919f11449449114494491144919f111999ffffffffff
;; 053:ffffffff911444991144499911444999114449991144499991144499ffffffff
;; 054:ffffffffffff1119fff44199f9441199f9441199fff44199ffff1119ffffffff
;; 064:ff000ffffffff0fffffff333ffff3777fff33777fff37777f337777733077777
;; 065:ff00ffff33ffffff333fffff7333000f77733fff77733fff77773fff77773fff
;; 066:fffff00ffffff0fffffff333ffff3777fff33777fff37777f337777733077777
;; 067:f0fff00f33fff0ff333f0fff73330fff77733fff77733fff77773fff77773fff
;; 068:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 069:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 070:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; 080:33077777f3377777fff33777fff33777ffff3773fff03333f00fffff00fffff0
;; 081:77773fff77773fff77733fff7773ffff333000ffffff00000fffffff0fffffff
;; 082:33077777f3377777fff33777fff33777ffff3773fff03333ffff0fffffff0ff0
;; 083:77773fff77773fff77733fff7773ffff330fffffff0fffff0f0fffff0f0fffff
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
;; 000:191d1e3f455070787ac0bfc0eae5e2baaa91877459534b3ad9af80b079726868877f9bb0bfd4b090b870628c70333c57
;; </PALETTE>

