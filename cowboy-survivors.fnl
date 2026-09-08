;; title:   Cowboy Survivors
;; author:  TofuTheLoafu
;; desc:    Vampire Survivors-like but western themed with tic80
;; site:    website link
;; license: MIT License (change this to your license of choice)
;; version: 0.1
;; script:  fennel
;; Cowboy Survivors - rewritten from s7 Scheme into Fennel for TIC-80.
(local screen-width 240)
(local screen-height 136)

(var t 0)

(var ui-module nil)
(var transition-scene nil)
(var main-menu-scene nil)

(local coroutines
  (do
    (var coroutine-list [])

    (fn make-coroutine [thunk]
      (coroutine.create (fn [] (thunk coroutine.yield))))

    (fn spawn! [thunk]
      (table.insert coroutine-list (make-coroutine thunk)))

    (fn pump-coroutines! []
      (let [to-run coroutine-list]
        (set coroutine-list [])
        (each [_ co (ipairs to-run)]
          (coroutine.resume co)
          (when (not= (coroutine.status co) :dead)
            (table.insert coroutine-list co)))))

    {: spawn! : pump-coroutines!}))

(local pool
  (do
    (fn make-pool [size]
      (let [free-stack {}
            in-use {}]
        (for [i 0 (- size 1)]
          (tset free-stack i i)
          (tset in-use i false))
        {:free-stack free-stack :in-use in-use :top size :size size}))

    (fn pool-add! [p obj]
      (if (= p.top 0)
          false
          (let [new-top (- p.top 1)
                i (. p.free-stack new-top)]
            (tset p :top new-top)
            (tset p.in-use i obj)
            i)))

    (fn pool-remove! [p i]
      (when (. p.in-use i)
        (tset p.in-use i false)
        (tset p.free-stack p.top i)
        (tset p :top (+ p.top 1))
        true))

    (fn pool-count [p]
      (- p.size p.top))

    (fn pool-for-each [p f]
      (for [i 0 (- p.size 1)]
        (let [e (. p.in-use i)]
          (when e (f e i)))))

    {: make-pool : pool-add! : pool-remove! : pool-count : pool-for-each})) 

(local utility
  (do
    (fn filter [pred lst]
      (let [result []]
        (each [_ v (ipairs lst)]
          (when (pred v) (table.insert result v)))
        result))

    (fn list-ref [lst i]
      (. lst (+ i 1)))

    (fn randi [n]
      (math.random 0 (- n 1)))

    (fn randf [n]
      (* (math.random) n))

    (fn list->random [lst]
      (if (= (length lst) 0)
          false
          (list-ref lst (randi (length lst)))))

    (fn id [x] x)

    (fn round [x]
      (math.floor (+ x 0.5)))

    (fn for-each-indexed [f seq]
      (var i 0)
      (each [_ v (ipairs seq)]
        (f v i)
        (set i (+ i 1))))

    (fn lerp [progress start end]
      (+ start (* progress (- end start))))

    (fn ease-out-cubic [p]
      (let [f (- p 1)] (+ 1 (* f f f))))

    (fn ease-in-cubic [p] (* p p p))

    (fn ease-in-out-cubic [p]
      (if (< p 0.5)
          (* 4 p p p)
          (let [f (- (* 2 p) 2)]
            (+ 1 (/ (* f f f) 2)))))

    (fn shuffle [arr]
      (let [copy []]
        (each [_ v (ipairs arr)] (table.insert copy v))
        (for [i (length copy) 2 -1]
          (let [j (+ 1 (math.random i))
                tmp (. copy i)]
            (tset copy i (. copy j))
            (tset copy j tmp)))
        copy))


    {: filter : list-ref : randi : randf : list->random : id : round
     : for-each-indexed : lerp : ease-out-cubic : ease-in-cubic
     : ease-in-out-cubic : shuffle }))

(local scene-module
  (do
    (var scenes [])

    (fn push-scene! [scene]
      (table.insert scenes 1 scene))

    (fn pop-scene! []
      (table.remove scenes 1))

    (fn set-scene! [scene]
      (set scenes [scene]))

    (fn swap-below! [new-scene]
      (tset scenes 2 new-scene))

    (fn current-scene []
      (. scenes 1))

    (fn run-scenes! []
      (when (> (length scenes) 0)
        (let [top (current-scene)]
          (top.input)
          (top.update)
          (for [i (length scenes) 1 -1]
            (let [s (. scenes i)]
              (s.render))))))

    {: push-scene! : pop-scene! : set-scene! : swap-below!
     : current-scene : run-scenes!}))

(local camera
  (do
    (local state {:x 0 :y 0 :target-x 10 :target-y 10 :cx 120 :cy 68})

    (fn update-camera! []
      (tset state :x (utility.lerp 0.05 state.x state.target-x))
      (tset state :y (utility.lerp 0.05 state.y state.target-y)))

    (fn camera-transform [wx wy]
      (values (math.floor (+ state.cx (- wx state.x)))
              (math.floor (+ state.cy (- wy state.y)))))

    {:state state : update-camera! : camera-transform}))

(local events
  (do
    (var event-list [])

    (fn add-event! [e]
      (table.insert event-list e))

    (fn process-events [f]
      (let [to-process event-list]
        (set event-list [])
        (each [_ e (ipairs to-process)] (f e))))

    {: add-event! : process-events}))

(local world
  (do
    (fn generate-map [width]
      (let [game-map {}
            center (/ (- width 1) 2.0)
            radius (- (/ width 2.0) 2)]
        (for [i 0 (- (* width width) 1)]
          (let [x (% i width)
                y (// i width)
                dx (- x center)
                dy (- y center)
                dist (math.sqrt (+ (* dx dx) (* dy dy)))]
            (tset game-map i (if (<= dist radius)
                                  (utility.list->random [2 4])
                                  :empty))))
        {:map game-map :width width :height width}))

    (fn floor-quotient [n d]
      (// n d))

    (fn map-get-cell [m x y]
      (. m.map (+ x (* y m.width))))

    (fn world->real-coords [x y]
      {:x (* 16 x) :y (* 16 y)})

    (fn real-coords->world [x y]
      {:x (floor-quotient x 16) :y (floor-quotient y 16)})

    (fn random-direction []
      (let [dir (* math.pi (utility.randf 2.0))]
        {:x (math.cos dir) :y (math.sin dir)}))

    (fn map-render [map-table cam]
      (let [game-map map-table.map
            map-width map-table.width
            map-height map-table.height
            margin 1
            cam-tile-x (- (floor-quotient (utility.round cam.x) 16)
                          (floor-quotient (utility.round cam.cx) 16) margin)
            cam-tile-y (- (floor-quotient (utility.round cam.y) 16)
                          (floor-quotient (utility.round cam.cy) 16) margin)
            span-x 18
            span-y 12]

        (fn in-bounds? [x y]
          (and (<= 0 x) (< x map-width) (<= 0 y) (< y map-height)))

        (for [i 0 (- (* span-x span-y) 1)]
          (let [screen-x (% i span-x)
                screen-y (// i span-x)
                world-x (+ cam-tile-x screen-x)
                world-y (+ cam-tile-y screen-y)]
            (when (in-bounds? world-x world-y)
              (let [(sx sy) (camera.camera-transform (* 16 world-x) (* 16 world-y))]
                (if (= (map-get-cell map-table world-x world-y) :empty)
                    (rect sx sy 16 16 6)
                    (spr (map-get-cell map-table world-x world-y) sx sy -1 1 0 0 2 2))))))

        (for [i 0 (- (* span-x span-y) 1)]
          (let [screen-x (- (% i span-x) 1)
                screen-y (- (// i span-x) 1)
                world-x (+ cam-tile-x screen-x)
                world-y (+ cam-tile-y screen-y)]
            (when (or (not (in-bounds? world-x world-y))
                      (= (map-get-cell map-table world-x world-y) :empty))
              (let [(sx sy) (camera.camera-transform (* 16 world-x) (* 16 world-y))]
                (each [_ offset (ipairs [[-4 -4 0] [4 4 0]])]
                  (spr 64 (+ (. offset 1) sx) (+ (. offset 2) sy)
                           15 1 0 (. offset 3) 2 2))))))))

    {: generate-map : floor-quotient : map-get-cell : world->real-coords
     : real-coords->world : random-direction : map-render}))

(local geometry
  (do
    (fn vec2 [x y] {:x x :y y})
    (fn vec2-x [v] v.x)
    (fn vec2-y [v] v.y)
    (fn vec2-add [a b] (vec2 (+ a.x b.x) (+ a.y b.y)))
    (fn vec2-sub [a b] (vec2 (- a.x b.x) (- a.y b.y)))
    (fn vec2-scale [a s] (vec2 (* a.x s) (* a.y s)))

    (fn overlapping? [a b]
      (let [dist-x (- a.x b.x)
            dist-y (- a.y b.y)
            dist-sq (+ (* dist-x dist-x) (* dist-y dist-y))
            min-dist (+ a.radius b.radius)]
        (< dist-sq (* min-dist min-dist))))

    (fn within-raw? [x1 y1 x2 y2 dist]
      (let [dist-x (- x1 x2)
            dist-y (- y1 y2)
            dist-sq (+ (* dist-x dist-x) (* dist-y dist-y))]
        (<= dist-sq (* dist dist))))

    (fn within? [a b dist]
      (within-raw? a.x a.y b.x b.y dist))

    (fn direction-to [from to]
      (let [dx (- to.x from.x)
            dy (- to.y from.y)
            dist-sq (+ (* dx dx) (* dy dy))]
        (if (= dist-sq 0)
            {:x 1.0 :y 0.0}
            (let [dist (math.sqrt dist-sq)]
              {:x (/ dx dist) :y (/ dy dist)}))))

    {: vec2 : vec2-x : vec2-y : vec2-add : vec2-sub : vec2-scale
     : overlapping? : within-raw? : within? : direction-to}))

(local collision
  (do
    (fn remove-entity [lst e]
      (let [result []]
        (each [_ v (ipairs lst)]
          (when (not= v e) (table.insert result v)))
        result))

    (fn push-apart! [e other]
      (let [dist-x (- e.x other.x)
            dist-y (- e.y other.y)
            dist-sq (+ (* dist-x dist-x) (* dist-y dist-y))
            min-dist (+ e.radius other.radius)
            zero-dist? (= dist-sq 0)
            dist (if zero-dist? 0.0 (math.sqrt dist-sq))
            dir-x (if zero-dist? 1.0 (/ dist-x dist))
            dir-y (if zero-dist? 0.0 (/ dist-y dist))
            overlap (* 0.5 (- min-dist dist))
            push-x (* overlap dir-x)
            push-y (* overlap dir-y)]
        (tset e :x (+ e.x push-x))
        (tset e :y (+ e.y push-y))
        (tset other :x (- other.x push-x))
        (tset other :y (- other.y push-y))))

    (fn make-collision-world [cell-size]
      (let [grid {}
            members {}
            handlers {}]
        (var next-id-counter 0)

        (fn next-id []
          (set next-id-counter (+ next-id-counter 1))
          next-id-counter)

        (fn cell-key [x y]
          (+ (* (math.floor (/ x cell-size)) 100000) (math.floor (/ y cell-size))))

        (fn bucket-add! [key e]
          (let [existing (or (. grid key) [])]
            (table.insert existing e)
            (tset grid key existing)))

        (fn bucket-remove! [key e]
          (when (. grid key)
            (let [remaining (remove-entity (. grid key) e)]
              (tset grid key (if (= (length remaining) 0) nil remaining)))))

        (fn register! [e]
          (when (not e.collision-id)
            (tset e :collision-id (next-id))
            (tset members e.collision-id e)
            (let [key (cell-key e.x e.y)]
              (tset e :collision-key key)
              (bucket-add! key e))))

        (fn unregister! [e]
          (when e.collision-id
            (tset members e.collision-id nil)
            (when e.collision-key (bucket-remove! e.collision-key e))
            (tset e :collision-key nil)
            (tset e :collision-id nil)))

        (fn update-membership! [e]
          (let [new-key (cell-key e.x e.y)
                old-key e.collision-key]
            (when (not= old-key new-key)
              (when old-key (bucket-remove! old-key e))
              (bucket-add! new-key e)
              (tset e :collision-key new-key))))

        (fn on-collision! [a-type b-type handler]
          (tset handlers (.. a-type "|" b-type) handler))

        (fn dispatch-collision! [a b]
          (let [h1 (. handlers (.. a.type "|" b.type))
                h2 (. handlers (.. b.type "|" a.type))]
            (if h1 (h1 a b)
                h2 (h2 b a)
                nil)))

        (fn check-pair! [a b]
          (when (geometry.overlapping? a b)
            (if (and (= a.type :enemy) (= b.type :enemy))
                (push-apart! a b)
                (dispatch-collision! a b))))

        (local neighbor-offsets [[1 0] [1 1] [0 1] [-1 1]])

        (fn nearby-entities [x y radius]
          (let [cx (math.floor (/ x cell-size))
                cy (math.floor (/ y cell-size))
                cell-radius (math.max 1 (math.ceil (/ radius cell-size)))
                out []]
            (for [dx (- cell-radius) (- cell-radius 1)]
              (for [dy (- cell-radius) (- cell-radius 1)]
                (let [bucket (. grid (cell-key (* cell-size (+ cx dx)) (* cell-size (+ cy dy))))]
                  (when bucket
                    (each [_ e (ipairs bucket)] (table.insert out e))))))
            out))

        (fn find-and-dispatch-collisions []
          (each [key bucket (pairs grid)]
            (let [cx (math.floor (/ key 100000))
                  cy (- key (* cx 100000))]
              (for [i 1 (length bucket)]
                (for [j (+ i 1) (length bucket)]
                  (check-pair! (. bucket i) (. bucket j))))
              (each [_ offset (ipairs neighbor-offsets)]
                (let [neighbor-cell (. grid (cell-key (* cell-size (+ cx (. offset 1)))
                                                       (* cell-size (+ cy (. offset 2)))))]
                  (when neighbor-cell
                    (each [_ a (ipairs bucket)]
                      (each [_ b (ipairs neighbor-cell)]
                        (check-pair! a b)))))))))

        (fn resolve! []
          (each [_ e (pairs members)] (update-membership! e))
          (find-and-dispatch-collisions))

        {:register! register!
         :unregister! unregister!
         :on-collision! on-collision!
         :nearby-entities nearby-entities
         :resolve! resolve!}))

    {: push-apart! : make-collision-world : remove-entity}))

(local entity-helpers
  (do
    (fn update-hit-debounce [e]
      (when e.hit-debounce
        (if (< 0 e.hit-debounce)
            (tset e :hit-debounce (- e.hit-debounce 1))
            (do (tset e :hit-debounce nil)
                (tset e :white-flash? false)))))

    (fn update-duration [e]
      (when e.duration
        (tset e :duration (- e.duration 1))
        (when (<= e.duration 0)
          (tset e :destroy? true)
          (when e.collision-id
            (events.add-event! [:collision-unregister e])))))

    (fn update-time [e]
      (when e.time (tset e :time (+ e.time 1))))

    (fn update-direction [e]
      (when e.direction
        (tset e :x (+ e.x (* e.direction.x (/ e.speed 10))))
        (tset e :y (+ e.y (* e.direction.y (/ e.speed 10))))))

    (fn update-entity [e game-time object-pool idx]
      (when e
        (update-duration e)
        (update-direction e)
        (update-time e)
        (update-hit-debounce e)
        (when e.destroy?
          (pool.pool-remove! object-pool idx))

        (if (= e.type :enemy)
            (do
              (when (and e.hp (<= e.hp 0))
                (when e.drop-xp
                  (events.add-event! [:spawn-xp e.x e.y e.drop-xp]))
                (tset e :destroy? true)
                (events.add-event! [:collision-unregister e]))
              (when e.update (e.update e)))

            (or (= e.type :projectile) (= e.type :boss))
            (e.update e)

            (= e.type :weapon)
            (e.update e game-time))))

    {: update-hit-debounce : update-duration : update-time
     : update-direction : update-entity}))

(local enemies
  (do
    (fn enemy-follow-player [e]
      (let [dir (geometry.direction-to e e.target)]
        (tset e :x (+ e.x (* dir.x (/ e.speed 10))))
        (tset e :y (+ e.y (* dir.y (/ e.speed 10))))))

    (fn sprite-frame [e]
      (let [sprite e.sprite]
        (if (= (type sprite) :table)
            (utility.list-ref sprite (% (// e.time 10) (length sprite)))
            sprite)))

    (fn draw-sprite-frame [e frame]
      (let [(sx sy) (camera.camera-transform e.x e.y)]
        (if (= (type frame) :table)
            (spr (. frame 1) sx sy 15 (or e.scale 1) 0 0 (. frame 2) (. frame 3))
            (spr frame sx sy 15 (or e.scale 1) 0 0 1 1))))

    (fn render-entity [e]
      (if e.white-flash?
          (do
            (for [c 0 15] (poke4 (+ (* 0x3ff0 2) c) 4))
            (draw-sprite-frame e (sprite-frame e))
            (for [c 0 15] (poke4 (+ (* 0x3ff0 2) c) c)))
          (draw-sprite-frame e (sprite-frame e))))

    (fn make-tumbleweed [x y target]
      {:type :enemy :x x :y y :radius 4 :speed 2 :time 0 :target target
       :update enemy-follow-player :sprite [304 305] :render render-entity
       :drop-xp 1 :hp 10})

    (fn make-big-tumbleweed [x y target]
      {:type :enemy :x x :y y :radius 8 :speed 1 :time 0 :target target
       :update enemy-follow-player :scale 2 :render render-entity
       :sprite [304 305] :drop-xp 1 :hp 20})

    (fn scarab-queen-update [e]
      (if (not (geometry.within? e.target e 72))
          (let [dir (geometry.direction-to e e.target)]
            (tset e :x (+ e.x (* dir.x (/ e.speed 10))))
            (tset e :y (+ e.y (* dir.y (/ e.speed 10)))))
          (when (= 0 (% e.time 240))
            (events.add-event! [:spawn-scarab e.x e.y e.target])
            (events.add-event! [:spawn-scarab e.x e.y e.target])
            (events.add-event! [:spawn-scarab e.x e.y e.target])
            (events.add-event! [:spawn-scarab e.x e.y e.target]))))

    (fn make-scarab-queen [x y target]
      {:type :enemy :x x :y y :radius 8 :speed 2 :time 0 :target target
       :update scarab-queen-update :render render-entity :scale 1
       :sprite [[320 2 2] [322 2 2]] :drop-xp 4 :hp 15})

    (fn make-scarab [x y target]
      {:type :enemy :x x :y y :radius 4 :speed 4 :time 0 :target target
       :update enemy-follow-player :render render-entity :scale 1
       :sprite 306 :hp 1})

    (fn cactus-update [e]
      (if (not (geometry.within? e.target e 70))
          (let [dir (geometry.direction-to e e.target)]
            (tset e :status :waddling)
            (tset e :sprite [[352 2 2] [354 2 2]])
            (tset e :x (+ e.x (* dir.x (/ e.speed 10))))
            (tset e :y (+ e.y (* dir.y (/ e.speed 10)))))

          (and (= e.status :charging-up) (<= e.charging-up 0))
          (let [dir (geometry.direction-to e e.target)]
            (tset e :status :charging)
            (tset e :sprite [[358 2 2]])
            (tset e :dir-x dir.x)
            (tset e :dir-y dir.y))

          (= e.status :charging)
          (do
            (tset e :x (+ e.x (* e.dir-x (/ e.speed 2))))
            (tset e :y (+ e.y (* e.dir-y (/ e.speed 2)))))

          (= e.status :charging-up)
          (tset e :charging-up (- e.charging-up 1))

          (do
            (tset e :status :charging-up)
            (tset e :sprite [[352 2 2] [356 2 2]])
            (tset e :charging-up 120))))

    (fn make-cactus [x y target]
      {:type :enemy :x x :y y :radius 8 :speed 2 :time 0 :target target
       :update cactus-update :render render-entity :scale 1
       :sprite [[320 2 2] [322 2 2]] :drop-xp 4 :hp 50})

    (fn render-snake [snake]
      (each [_ link (ipairs snake.links)]
        (when link
          (let [(sx sy) (camera.camera-transform link.x link.y)]
            (if link.white-flash?
                (do
                  (for [c 0 15] (poke4 (+ (* 0x3ff0 2) c) 4))
                  (ui-module.draw-sprite sx sy 1 (- link.angle)
                                         (% link.sprite 16) (// link.sprite 16) 1 1 15)
                  (for [c 0 15] (poke4 (+ (* 0x3ff0 2) c) c)))
                (ui-module.draw-sprite sx sy 1 (- link.angle)
                                       (% link.sprite 16) (// link.sprite 16) 1 1 15))))))

    (fn update-snake [snake]
      (let [head (. snake.links 1)
            target-angle (math.atan (- snake.target.y head.y) (- snake.target.x head.x))
            angle-diff (- (% (+ (- target-angle head.angle) math.pi) (* 2 math.pi)) math.pi)
            max-turn 0.010
            turn-factor (/ (math.abs angle-diff) math.pi)
            speed (* 1 (- 1.0 (* 0.6 turn-factor)))]
        (tset head :angle (+ head.angle (math.max (- max-turn) (math.min max-turn angle-diff))))
        (tset head :x (+ head.x (* speed (math.cos head.angle))))
        (tset head :y (+ head.y (* speed (math.sin head.angle)))))

      (tset snake :destroy? true)

      (var prev-link (. snake.links 1))
      (for [i 2 (length snake.links)]
        (let [link (. snake.links i)]
          (when link
            (let [dx (- prev-link.x link.x)
                  dy (- prev-link.y link.y)
                  dist (math.sqrt (+ (* dx dx) (* dy dy)))]
              (when (not link.destroy?)
                (tset snake :destroy? false))
              (entity-helpers.update-hit-debounce link)
              (tset link :angle prev-link.angle)
              (when (< 8 dist)
                (tset link :x (+ link.x (* (/ dx dist) (- dist 8))))
                (tset link :y (+ link.y (* (/ dy dist) (- dist 8)))))

              (when (<= link.hp 0)
                (events.add-event! [:collision-unregister link])
                (events.add-event! [:spawn-xp link.x link.y link.drop-xp])
                (tset link :destroy? true))

              (if link.destroy?
                  (tset snake.links i false)
                  (set prev-link link))))))

      nil)

    (fn make-snake-link [tail?]
      {:type :link :enemy-type :snake-link :x 0 :y 0 :radius 8 :angle 0
       :drop-xp 1 :sprite (if tail? 310 309) :hp 50})

    (fn make-snake-head [x y]
      {:type :link-head :x x :y y :speed 0 :radius 8 :angle 0 :sprite 308})

    (fn make-snake [x y target]
      (let [links []]
        (table.insert links (make-snake-head x y))
        (for [i 2 19]
          (table.insert links (make-snake-link false)))
        (table.insert links (make-snake-link true))
        {:type :boss :update update-snake :render render-snake :target target :links links}))

    (fn damage-enemy! [e amount]
      (if e.hit-debounce
          false
          (do
            (tset e :hp (- e.hp amount))
            (tset e :hit-debounce 8)
            (tset e :white-flash? true)
            true)))

    {: enemy-follow-player : sprite-frame : draw-sprite-frame : render-entity
     : make-tumbleweed : make-big-tumbleweed : scarab-queen-update
     : make-scarab-queen : make-scarab : cactus-update : make-cactus
     : render-snake : update-snake : make-snake-link : make-snake-head
     : make-snake : damage-enemy!}))

(local gem-module
  (do
    (fn make-xp-orb [x y] [x y 259])
    (fn gem-x [g] (. g 1))
    (fn gem-y [g] (. g 2))
    (fn gem-sprite [g] (. g 3))

    (fn render-gem [g]
      (let [(sx sy) (camera.camera-transform (gem-x g) (gem-y g))]
        (spr (gem-sprite g) sx sy 15 1 0 0 1 1)))

    (fn update-gem [g cowboy collect-xp!]
      (if (geometry.within-raw? (gem-x g) (gem-y g) cowboy.x cowboy.y cowboy.collection-radius)
          (do (collect-xp! cowboy 1) nil)
          g))

    {: make-xp-orb : gem-x : gem-y : gem-sprite : render-gem : update-gem}))

(set ui-module
  (do
    (fn render-time [game-time]
      (let [seconds (// game-time 60)]
        (print (.. "Time " (tostring seconds)) 195 2 0)
        (print (.. "Time " (tostring seconds)) 196 2 4)))

    (fn render-hp [hp]
      (for [i 0 (- hp 1)]
        (spr 272 (* i 9) 0 15 1 0 0 1 1)))

    (fn render-xp [xp xp-needed]
      (rect 0 (- screen-height 4) (math.floor (* screen-width (/ xp xp-needed))) 4 13))

    (fn shake-effect! [intensity ?duration]
      (let [duration (or ?duration 20)]
        (coroutines.spawn!
          (fn [yield]
            (for [i 0 (- duration 1)]
              (poke 0x3FF9 (- (utility.randi (+ 1 (* 2 intensity))) intensity))
              (poke (+ 0x3FF9 1) (- (utility.randi (+ 1 (* 2 intensity))) intensity))
              (yield))
            (memset 0x3FF9 0 2)))))

    (fn draw-sprite [sx sy scale angle mx my mw mh key]
      (let [sv [[-1 -1 0 0] [1 -1 0.999 0] [-1 1 0 0.999] [1 1 0.999 0.999]]
            scale-x (* (lshift mw 2) scale)
            scale-y (* (lshift mh 2) scale)
            a (* -1 angle)]
        (for [p 1 4]
          (let [point (. sv p)
                _sx (* (. point 1) scale-x)
                _sy (* (. point 2) scale-y)
                rx (- (* _sx (math.cos a)) (* _sy (math.sin a)))
                ry (+ (* _sx (math.sin a)) (* _sy (math.cos a)))]
            (tset point 1 (utility.round (+ rx sx)))
            (tset point 2 (utility.round (+ ry sy)))
            (tset point 3 (utility.round (+ (lshift mx 3) (* (. point 3) (lshift mw 3)))))
            (tset point 4 (utility.round (+ (lshift my 3) (* (. point 4) (lshift mh 3)))))))
        (ttri (. (. sv 1) 1) (. (. sv 1) 2)
                  (. (. sv 2) 1) (. (. sv 2) 2)
                  (. (. sv 3) 1) (. (. sv 3) 2)
                  (. (. sv 1) 3) (. (. sv 1) 4)
                  (. (. sv 2) 3) (. (. sv 2) 4)
                  (. (. sv 3) 3) (. (. sv 3) 4) 0 key)
        (ttri (. (. sv 2) 1) (. (. sv 2) 2)
                  (. (. sv 3) 1) (. (. sv 3) 2)
                  (. (. sv 4) 1) (. (. sv 4) 2)
                  (. (. sv 2) 3) (. (. sv 2) 4)
                  (. (. sv 3) 3) (. (. sv 3) 4)
                  (. (. sv 4) 3) (. (. sv 4) 4) 0 key)))

    {: render-time : render-hp : render-xp : shake-effect! : draw-sprite}))

(local projectile
  (do
    (fn update-bullet [e] true)

    (fn render-bullet [self]
      (let [(sx sy) (camera.camera-transform self.x self.y)]
        (spr self.sprite sx sy 15 1 0 0 1 1)))

    (fn make-projectile [x y direction speed damage]
      {:x x :y y :radius 6 :speed speed :type :projectile :direction direction
       :sprite 258 :duration 120 :damage damage :update update-bullet :render render-bullet})

    {: update-bullet : render-bullet : make-projectile}))

(local damage-item
  (do
    (fn damage-blueprint []
      {:id :damage-item :type :upgrade :sprite 262 :name "Damage" :damage 1
       :stats [:damage] :description "Gives extra damage"})

    (fn make-damage-item [cowboy]
      (let [new-item (damage-blueprint)]
        (tset cowboy :extra-damage (+ 1 cowboy.extra-damage))
        new-item))

    {: damage-blueprint : make-damage-item}))

(local xp-radius-item
  (do
    (fn xp-radius-blueprint []
      {:id :xp-radius :type :upgrade :sprite 259 :name "XP Radius"
       :description "Increases gem pickup radius"})

    (fn make-xp-radius-item [cowboy]
      (let [new-item (xp-radius-blueprint)]
        (tset cowboy :collection-radius (+ 8 cowboy.collection-radius))
        new-item))

    {: xp-radius-blueprint : make-xp-radius-item}))

(local speed-item
  (do
    (fn speed-blueprint []
      {:id :speed-item :type :upgrade :sprite 261 :name "Speed"
       :description "Gives extra speed"})

    (fn make-speed-item [cowboy]
      (let [new-item (speed-blueprint)]
        (tset cowboy :speed (+ 2 cowboy.speed))
        new-item))

    {: speed-blueprint : make-speed-item}))

(local extra-heart
  (do
    (fn heart-blueprint []
      {:id :heart :type :upgrade :sprite 272 :name "Heart"
       :description "Gives 1 extra life"})

    (fn make-extra-heart [cowboy]
      (let [new-item (heart-blueprint)]
        (tset cowboy :hp (+ 1 cowboy.hp))
        new-item))

    {: heart-blueprint : make-extra-heart}))

(local playing-card
  (do
    (fn playing-card-position [cowboy tt index]
      (let [angle (+ (* tt 0.05) (* index (/ (* 2 math.pi) cowboy.playing-card-count)))
            dir-vector (geometry.vec2 (math.cos angle) (math.sin angle))
            cowboy-vec (geometry.vec2 cowboy.x cowboy.y)
            offset (geometry.vec2-scale dir-vector 32)]
        (geometry.vec2-add offset cowboy-vec)))

    (fn update-playing-card [pc game-time]
      (let [position (playing-card-position pc.cowboy game-time pc.index)]
        (tset pc :x (math.floor (geometry.vec2-x position)))
        (tset pc :y (math.floor (geometry.vec2-y position)))))

    (fn render-playing-card [self]
      (let [(sx sy) (camera.camera-transform self.x self.y)]
        (spr self.sprite sx sy 15 1 0 0 1 1)))

    (fn make-playing-card [cowboy index]
      {:x cowboy.x :y cowboy.y :cowboy cowboy :radius 8 :type :weapon :sprite 263
       :index index :damage 1 :update update-playing-card :render render-playing-card})

    (fn update-playing-card-blueprint [self object-pool collision-world]
      (when (not self.playing-card)
        (let [pc-count (length (utility.filter (fn [u] (= u.id :playing-card)) self.player.upgrades))
              new-card (make-playing-card self.player pc-count)]
          (tset self.player :playing-card-count pc-count)
          (tset self :playing-card new-card)
          (pool.pool-add! object-pool new-card)
          (collision-world.register! new-card))))

    (fn playing-card-blueprint []
      {:id :playing-card :type :upgrade :name "Texas \nHold-em" :sprite 263
       :range 32 :playing-card false :damage 1 :stats [:damage]
       :update update-playing-card-blueprint})

    (fn make-playing-card-gear [cowboy]
      (let [bp (playing-card-blueprint)]
        (tset bp :player cowboy)
        bp))

    {: playing-card-position : update-playing-card : render-playing-card
     : make-playing-card : update-playing-card-blueprint : playing-card-blueprint
     : make-playing-card-gear}))

(local winchester
  (do
    (fn update-winchester [self object-pool collision-world]
      (tset self :cooldown (- self.cooldown 1))
      (let [cowboy self.player]
        (when (<= self.cooldown 0)
          (each [_ dir (ipairs [[1.0 1.0] [1.0 -1.0] [-1.0 -1.0] [-1.0 1.0]])]
            (let [bullet (projectile.make-projectile cowboy.x cowboy.y
                           {:x (. dir 1) :y (. dir 2)}
                           20 (+ cowboy.extra-damage self.damage))]
              (pool.pool-add! object-pool bullet)
              (collision-world.register! bullet)))
          (tset self :cooldown self.max-cooldown))))

    (fn winchester-blueprint []
      {:id :winchester :type :upgrade :sprite 264 :name "Winchester"
       :description "Shoots in a diagonal pattern" :cooldown 200 :max-cooldown 200
       :damage 4 :stats [:max-cooldown :damage] :update update-winchester})

    (fn make-winchester [cowboy]
      (let [bp (winchester-blueprint)]
        (tset bp :player cowboy)
        bp))

    {: update-winchester : winchester-blueprint : make-winchester}))

(local revolver
  (do
    (fn render-revolver [self]
      (when self.target
        (let [(sx sy) (camera.camera-transform (- self.target.x 3) (- self.target.y 3))]
          (spr 257 sx sy 15 2 0 0 1 1))))

    (fn update-revolver [self object-pool collision-world]
      (let [cowboy self.player]
        (when (and self.target self.target.destroy?)
          (tset self :target nil))
        (if self.target
            (tset self :cooldown (- self.cooldown 1))
            (let [candidates (utility.filter
                                (fn [e] (and (or (= e.type :enemy) (= e.type :link))
                                             (geometry.within? cowboy e self.range)))
                                (collision-world.nearby-entities cowboy.x cowboy.y self.range))]
              (tset self :target (utility.list->random candidates))))

        (when (and self.target (<= self.cooldown 0))
          (let [bullet (projectile.make-projectile cowboy.x cowboy.y
                         (geometry.direction-to cowboy self.target)
                         40 (+ cowboy.extra-damage self.damage))]
            (pool.pool-add! object-pool bullet)
            (collision-world.register! bullet))
          (tset self :target nil)
          (tset self :cooldown self.max-cooldown))))

    (fn revolver-blueprint []
      {:id :revolver :type :upgrade :sprite 260 :name "Revolver"
       :description "Targets 1 enemy at a time, fires a bullet" :cooldown 120
       :max-cooldown 120 :range 64 :target false :damage 9
       :stats [:max-cooldown :range :damage] :update update-revolver :render render-revolver})

    (fn make-revolver [cowboy]
      (let [bp (revolver-blueprint)]
        (tset bp :player cowboy)
        bp))

    {: render-revolver : update-revolver : revolver-blueprint : make-revolver}))

(local level-up-scene
  (do
    (local gear-catalog
      [{:id :revolver :blueprint (revolver.revolver-blueprint) :make revolver.make-revolver}
       {:id :winchester :blueprint (winchester.winchester-blueprint) :make winchester.make-winchester}
       {:id :heart :blueprint (extra-heart.heart-blueprint) :make extra-heart.make-extra-heart}
       {:id :speed-item :blueprint (speed-item.speed-blueprint) :make speed-item.make-speed-item}
       {:id :xp-radius :blueprint (xp-radius-item.xp-radius-blueprint) :make xp-radius-item.make-xp-radius-item}
       {:id :damage-item :blueprint (damage-item.damage-blueprint) :make damage-item.make-damage-item}
       {:id :playing-card :blueprint (playing-card.playing-card-blueprint) :make playing-card.make-playing-card-gear}])

    (fn choose-upgrade-options []
      (let [shuffled (utility.shuffle gear-catalog)]
        [(. shuffled 1) (. shuffled 2) (. shuffled 3)]))

    (fn apply-gear! [cowboy upgrade]
      (let [new-upgrade ((. upgrade :make) cowboy)]
        (table.insert cowboy.upgrades 1 new-upgrade)
        cowboy))

    (fn stat-display-name [stat]
      (if (= stat :max-cooldown) "cooldown" stat))

    (fn render-level-up-menu [selected options]
      (rect 13 13 (- screen-width 26) (- screen-height 26) 0)
      (rect 15 15 (- screen-width 30) (- screen-height 30) 7)
      (print "Choose an upgrade" 17 17 0)
      (print "Choose an upgrade" 18 18 3)
      (utility.for-each-indexed
        (fn [option i]
          (let [card-height (- screen-height 60)
                card-width (- (// screen-width 3) 20)
                card-start-y 30
                card-start-x (+ 25 (* i (+ 5 card-width)))
                bp option.blueprint]
            (when (= i selected)
              (rect (- card-start-x 3) (- card-start-y 3) (+ card-width 6) (+ card-height 6) 8))
            (rect card-start-x card-start-y card-width card-height 3)
            (spr bp.sprite (+ 2 card-start-x) (+ 2 card-start-y) 15 2 0 0 1 1)
            (print bp.name (+ 2 card-start-x) (+ 20 card-start-y) 0)
            (when bp.stats
              (utility.for-each-indexed
                (fn [stat j]
                  (print (stat-display-name stat) (+ 2 card-start-x) (+ 36 (* j 10) card-start-y) 0 true 1 true)
                  (print (tostring (. bp stat)) (+ 42 card-start-x) (+ 36 (* j 10) card-start-y) 0))
                bp.stats))))
        options))

    (fn make-level-up-scene [cowboy]
      (let [options (choose-upgrade-options)]
        (var selected 0)
        {:input (fn []
                  (when (btnp 3) (set selected (% (+ selected 1) (length options))))
                  (when (btnp 2) (set selected (% (- selected 1) (length options))))
                  (when (btnp 4)
                    (apply-gear! cowboy (. options (+ selected 1)))
                    (scene-module.pop-scene!)))
         :update (fn [] true)
         :render (fn [] (render-level-up-menu selected options))}))

    {: gear-catalog : choose-upgrade-options : apply-gear! : render-level-up-menu
     : make-level-up-scene}))

(local cowboy-module
  (do
    (local cowboy-sprite 256)

    (fn damage-cowboy! [e amount]
      (when (not e.hit-debounce)
        (tset e :hp (- e.hp amount))
        (ui-module.shake-effect! 2 10)
        (tset e :hit-debounce 50)
        (tset e :white-flash? true)
        (coroutines.spawn!
          (fn [yield]
            (for [i 0 5] (yield))
            (tset e :white-flash? false)))))

    (fn render-cowboy [cowboy]
      (let [(esx esy) (camera.camera-transform cowboy.x (+ 8 cowboy.y))]
        (elli esx esy 7 3 0))
      (let [y-offset (+ -2 (* 2 (math.sin (/ t 3))))
            rotation (* 0.1 (math.sin (* t 0.1)))]
        (when cowboy.white-flash?
          (for [c 0 15] (poke4 (+ (* 0x3ff0 2) c) 4)))

        (if cowboy.moving?
            (let [(csx csy) (camera.camera-transform cowboy.x (+ y-offset cowboy.y))]
              (ui-module.draw-sprite csx csy 2 (* math.pi rotation) 0 16 1 1 15))
            (let [(csx csy) (camera.camera-transform cowboy.x cowboy.y)]
              (ui-module.draw-sprite csx csy 2 0 0 16 1 1 15)))

        (when cowboy.white-flash?
          (for [c 0 15] (poke4 (+ (* 0x3ff0 2) c) c)))))

    (fn xp-needed [level] (+ 2 (* level 3)))

    (fn level-up! [cowboy]
      (tset cowboy :level (+ 1 cowboy.level))
      (tset cowboy :xp 0)
      (scene-module.push-scene! (level-up-scene.make-level-up-scene cowboy)))

    (fn collect-xp! [cowboy amount]
      (tset cowboy :xp (+ amount cowboy.xp))
      (when (<= (xp-needed cowboy.level) cowboy.xp)
        (level-up! cowboy)))

    (fn move-cowboy-x! [game-map cowboy pos?]
      (let [new-x (if pos? (+ cowboy.x (* cowboy.speed 0.05)) (- cowboy.x (* cowboy.speed 0.05)))
            wc (world.real-coords->world (utility.round new-x) (utility.round cowboy.y))
            tile (world.map-get-cell game-map wc.x wc.y)]
        (when (not= tile :empty)
          (tset cowboy :x new-x))))

    (fn move-cowboy-y! [game-map cowboy pos?]
      (let [new-y (if pos? (+ cowboy.y (* cowboy.speed 0.05)) (- cowboy.y (* cowboy.speed 0.05)))
            wc (world.real-coords->world (utility.round cowboy.x) (utility.round new-y))
            tile (world.map-get-cell game-map wc.x wc.y)]
        (when (not= tile :empty)
          (tset cowboy :y new-y))))

    (fn make-cowboy [world-x world-y]
      (let [coords (world.world->real-coords world-x world-y)]
        {:type :player :x coords.x :y coords.y :radius 6 :collection-radius 16
         :speed 10 :level 1 :xp 0 :extra-damage 0 :upgrades []
         :update (fn [x y] false) :hp 5 :render render-cowboy :moving? false}))

    {: cowboy-sprite : damage-cowboy! : render-cowboy : xp-needed : level-up!
     : collect-xp! : move-cowboy-x! : move-cowboy-y! : make-cowboy}))

(local collision-rules
  (do
    (fn register-collision-rules! [collision-world]
      (collision-world.on-collision! :projectile :link
        (fn [a b]
          (when (enemies.damage-enemy! b a.damage)
            (tset a :destroy? true)
            (events.add-event! [:collision-unregister a]))))

      (collision-world.on-collision! :weapon :link
        (fn [w e] (enemies.damage-enemy! e (+ w.cowboy.extra-damage w.damage))))

      (collision-world.on-collision! :projectile :enemy
        (fn [a b]
          (when (enemies.damage-enemy! b a.damage)
            (tset a :destroy? true)
            (events.add-event! [:collision-unregister a]))))

      (collision-world.on-collision! :weapon :enemy
        (fn [w e] (enemies.damage-enemy! e (+ w.cowboy.extra-damage w.damage))))

      (collision-world.on-collision! :player :link
        (fn [a b] (cowboy-module.damage-cowboy! a 1)))

      (collision-world.on-collision! :player :enemy
        (fn [a b] (cowboy-module.damage-cowboy! a 1))))

    {: register-collision-rules!}))

(local enemy-waves
  (do
    (local enemy-types
      [{:id :tumbleweed :cost 1 :min-time 0 :make enemies.make-tumbleweed}
       {:id :big-tumbleweed :cost 3 :min-time 40 :make enemies.make-big-tumbleweed}
       {:id :scarab-queen :cost 7 :min-time 80 :make enemies.make-scarab-queen}
       {:id :cactus :cost 10 :min-time 130 :make enemies.make-cactus}])

    (fn available-enemies [elapsed]
      (utility.filter (fn [e] (<= e.min-time elapsed)) enemy-types))

    (fn current-budget [elapsed-seconds]
      (+ 500 (* 0.05 elapsed-seconds)))

    (fn spend-budget [budget elapsed]
      (let [candidates (available-enemies elapsed)
            picks []]
        (var continue? true)
        (var remaining budget)
        (while continue?
          (let [affordable (utility.filter (fn [e] (<= e.cost remaining)) candidates)]
            (if (= (length affordable) 0)
                (set continue? false)
                (let [choice (utility.list->random affordable)]
                  (set remaining (- remaining choice.cost))
                  (table.insert picks choice)))))
        picks))

    (fn get-random-position-radius-cowboy [cowboy radius]
      (let [dir (world.random-direction)]
        {:x (+ cowboy.x (* radius dir.x)) :y (+ cowboy.y (* radius dir.y))}))

    (fn spawn-enemies [object-pool collision-world cowboy tt]
      (let [seconds (/ tt 60)]  
        (when (= 0 (% seconds 5))
          (each [_ enemy (ipairs (spend-budget (current-budget seconds) seconds))]
            (let [coords (get-random-position-radius-cowboy cowboy 128)
                  entity ((. enemy :make) coords.x coords.y cowboy)]
              (pool.pool-add! object-pool entity)
              (when entity.radius
                (collision-world.register! entity)))))

        (when (and (<= 180 seconds) (= 0 (% seconds 50)))
          (let [snake (enemies.make-snake 0 0 cowboy)]
            (pool.pool-add! object-pool snake)
            (each [_ l (ipairs snake.links)]
              (when (= l.type :link)
                (collision-world.register! l)))))))

    {: enemy-types : available-enemies : current-budget : spend-budget
     : get-random-position-radius-cowboy : spawn-enemies}))

(local pause-menu
  (do
    (fn render-upgrade [upgrade selected x y]
      (let [w 16 h 16]
        (if selected
            (rect (- x 2) (- y 2) (+ 4 w) (+ 4 h) 14)
            (rect (- x 1) (- y 1) (+ 2 w) (+ 2 h) 0))
        (rect x y w h 4)
        (spr upgrade.sprite x y 15 2 0 0 1 1)))

    (fn render-upgrade-list [start-x start-y upgrades selected]
      (utility.for-each-indexed
        (fn [upgrade i]
          (let [row-width 6
                tile-y (// i row-width)
                tile-x (% i row-width)
                x (+ start-x (* tile-x 19))
                y (+ start-y (* tile-y 19))]
            (render-upgrade upgrade (= i selected) x y)))
        upgrades))

    (fn stat-display-name [stat]
      (if (= stat :max-cooldown) "cooldown" stat))

    (fn render-upgrade-info [start-x start-y upgrade]
      (rect start-x 15 (- screen-width start-x 17) (- screen-height 30) 0)
      (rect (+ 2 start-x) 15 (- screen-width start-x 17) (- screen-height 30) 4)
      (spr upgrade.sprite (+ 2 start-x) (+ 2 15) 15 2 0 0 1 1)
      (print upgrade.name (+ 2 start-x) (+ 20 15) 0)
      (when upgrade.stats
        (utility.for-each-indexed
          (fn [stat j]
            (print (stat-display-name stat) (+ 2 start-x) (+ 36 (* j 10) start-y) 0 true 1 true)
            (print (tostring (. upgrade stat)) (+ 42 start-x) (+ 36 (* j 10) start-y) 0))
          upgrade.stats)))

    (fn render-pause-menu [upgrades selected]
      (rect 13 13 (- screen-width 26) (- screen-height 26) 0)
      (rect 15 15 (- screen-width 30) (- screen-height 30) 7)
      (print "Pause" 17 17 0)
      (print "Pause" 18 18 3)
      (render-upgrade-info 130 15 (. upgrades (+ selected 1)))
      (render-upgrade-list 17 26 upgrades selected))

    (fn make-pause-menu [upgrades]
      (var selected 0)
      {:input (fn []
                (when (btnp 3) (set selected (% (+ selected 1) (length upgrades))))
                (when (btnp 2) (set selected (% (- selected 1) (length upgrades))))
                (when (btnp 5) (scene-module.pop-scene!)))
       :update (fn [] true)
       :render (fn [] (render-pause-menu upgrades selected))})

    {: render-upgrade : render-upgrade-list : render-upgrade-info
     : render-pause-menu : make-pause-menu}))

(local game-over-scene
  (do
    (fn render-game-over-scene []
      (print "Game Over" 30 30 2)
      (print "Game Over" 31 31 0)
      (print "Press X to continue" 31 61 0))

    (fn make-game-over-scene [on-restart]
      {:input (fn [] (when (btnp 5) (on-restart)))
       :update (fn [] true)
       :render render-game-over-scene})

    {: render-game-over-scene : make-game-over-scene}))

(local game-scene-module
  (do
    (var game-time 0)

    (fn make-game-scene []
      (set game-time 0)
      (var gems [])
      (let [game-map (world.generate-map 35)
            entity-pool (pool.make-pool 1000)
            collision-world (collision.make-collision-world 16)
            cowboy (cowboy-module.make-cowboy (// game-map.width 2) (// game-map.height 2))]
        (collision-rules.register-collision-rules! collision-world)
        (collision-world.register! cowboy)
        (pool.pool-add! entity-pool cowboy)

        (fn update-gem-once [gem]
          (gem-module.update-gem gem cowboy cowboy-module.collect-xp!))

        (fn update-gems []
          (set gems (utility.filter update-gem-once gems)))

        (tset camera.state :x cowboy.x)
        (tset camera.state :y cowboy.y)

        (fn input []
          (when (btn 3) (cowboy-module.move-cowboy-x! game-map cowboy true))
          (when (btn 1) (cowboy-module.move-cowboy-y! game-map cowboy true))
          (when (btn 2) (cowboy-module.move-cowboy-x! game-map cowboy false))
          (when (btn 0) (cowboy-module.move-cowboy-y! game-map cowboy false))

          (when (btnp 5)
            (scene-module.push-scene! (pause-menu.make-pause-menu cowboy.upgrades)))

          (if (or (btn 0) (btn 1) (btn 2) (btn 3))
              (tset cowboy :moving? true)
              (tset cowboy :moving? false)))

        (fn update []
          (when (< game-time 3)
            (cowboy-module.level-up! cowboy))

          (tset camera.state :target-x cowboy.x)
          (tset camera.state :target-y cowboy.y)
          (camera.update-camera!)

          (when (<= cowboy.hp 0)
            (scene-module.push-scene!
              (game-over-scene.make-game-over-scene
                (fn []
                  (scene-module.push-scene!
                    (transition-scene.make-transition-scene
                      (fn [] (scene-module.swap-below! (main-menu-scene.make-main-menu-scene)))
                      40))))))

          (enemy-waves.spawn-enemies entity-pool collision-world cowboy game-time)

          (pool.pool-for-each entity-pool
            (fn [e i] (entity-helpers.update-entity e game-time entity-pool i)))

          (each [_ upgrade (ipairs cowboy.upgrades)]
            (when upgrade.update
              (upgrade.update upgrade entity-pool collision-world)))

          (collision-world.resolve!)

          (update-gems)

          (events.process-events
            (fn [event]
              (let [kind (. event 1)]
                (if (= kind :collision-unregister)
                    (collision-world.unregister! (. event 2))

                    (= kind :spawn-xp)
                    (table.insert gems (gem-module.make-xp-orb (. event 2) (. event 3)))

                    (= kind :spawn-scarab)
                    (let [scarab (enemies.make-scarab (. event 2) (. event 3) (. event 4))]
                      (collision-world.register! scarab)
                      (pool.pool-add! entity-pool scarab))))))

          (set game-time (+ 1 game-time)))

        (fn render []
          (cls 0)
          (world.map-render game-map camera.state)

          (pool.pool-for-each entity-pool (fn [e i] (e.render e)))

          (each [_ gem (ipairs gems)] (gem-module.render-gem gem))

          (each [_ upgrade (ipairs cowboy.upgrades)]
            (when upgrade.render (upgrade.render upgrade)))

          (ui-module.render-hp cowboy.hp)
          (ui-module.render-time game-time)
          (ui-module.render-xp cowboy.xp (cowboy-module.xp-needed cowboy.level)))

        {:input input :update update :render render}))

    {: make-game-scene}))

(set transition-scene
  (do
    (fn render-transition-screen [x y]
      (for [i 0 14]
        (for [j 0 8]
          (spr 66 (+ x (* 16 i)) (+ y (* 16 j)) -1 1 0 0 2 2))))

    (fn make-transition-scene [on-swap duration]
      (var elapsed 0)
      (var phase :out)
      {:input (fn [] true)
       :update (fn []
                 (set elapsed (+ 1 elapsed))
                 (when (<= duration elapsed)
                   (if (= phase :out)
                       (do (on-swap) (set phase :in) (set elapsed 0))
                       (= phase :in)
                       (scene-module.pop-scene!))))
       :render (fn []
                 (let [progress (/ elapsed duration)]
                   (if (= phase :in)
                       (render-transition-screen 0 (math.floor (* screen-height (utility.ease-out-cubic progress))))
                       (render-transition-screen 0 (math.floor (- screen-height (* screen-height (utility.ease-out-cubic progress))))))))})

    {: render-transition-screen : make-transition-scene}))

(set main-menu-scene
  (do
    (fn menu-option [x y name selected?]
      (print name x y 0)
      (print name (+ 1 x) (+ 1 y) 4)
      (when selected?
        (tri x (+ 4 y) (- x 4) y (- x 4) (+ 8 y) 14)))

    (fn render-main-menu-scene [menu-options selected]
      (cls 0)
      (print "Cowboy Survivors v0.17" 5 5 0)
      (print "Cowboy Survivors v0.17" 6 6 4)
      (utility.for-each-indexed
        (fn [option i] (menu-option 30 30 option.text (= selected i)))
        menu-options))

    (fn make-main-menu-scene []
      (var selected 0)
      (let [menu-options
            [{:text "Play"
              :fn (fn []
                    (scene-module.push-scene!
                      (transition-scene.make-transition-scene
                        (fn [] (scene-module.swap-below! (game-scene-module.make-game-scene)))
                        40)))}]]
        {:input (fn []
                  (when (btnp 0) (set selected (% (+ selected 1) 1)))
                  (when (btnp 1) (set selected (% (- selected 1) 1)))
                  (when (btnp 4)
                    (let [opt (. menu-options (+ selected 1))]
                      (opt.fn))))
         :update (fn [] true)
         :render (fn [] (render-main-menu-scene menu-options selected))}))

    {: menu-option : render-main-menu-scene : make-main-menu-scene}))

(scene-module.push-scene! (main-menu-scene.make-main-menu-scene))

(fn _G.TIC []
  (coroutines.pump-coroutines!)
  (scene-module.run-scenes!)
  (set t (+ t 1)))

;; <TILES>
;; 002:6666666666666666666676766666676666666666666666666666666666666666
;; 003:6666666666666666666666666666666666666666666666666666666666666666
;; 004:6666666666666666666666666666666666666666666666666666666666666666
;; 005:6666666666666666666666666666666666666666666666666666666666666666
;; 018:6666666666666666666666666676766666676666666666666666666666666666
;; 019:6666666666676766666676666666666666666666666666666666666666666666
;; 020:6666666666666666666666666666666666666666666666666666666666666666
;; 021:6666666666666666666666666666666666666666666666666666666666666666
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
;; 007:f000000ff044440ff049440ff044440ff044440ff044940ff044440ff000000f
;; 008:fffffff2ffffff2ffffff27fffff27ffff227ffff771ffff7711ffff77ffffff
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
;; 096:fffffffffffffffffffffff5ff1ff557f111fff1f1e11111f1ec11eef1eeeee0
;; 097:ffffffff557fffff7777ffff777777ffeee1ffffeee1f111ece111e1e0eecee1
;; 098:fffffffffffffff5ffffff57ffff5577ff1fff1ef111f11ef1e111eef1ec1ee0
;; 099:ffffffff57ffffff777fffff77777fffee1ff111eee111e1ece1cee1e0eeee11
;; 100:fffffffffffffffffffffff5ff1ff557f111fff1f1c11111f1cc11ccf1ccccc0
;; 101:ffffffff557fffff7777ffff777777ffccc1ffffccc1f111ccc111c1c0ccccc1
;; 102:fffffffffffffff9ffffff99ffff9999ff1fff1ef111f00ef1e11100f1ec1ee9
;; 103:ffffffff99ffffff999fffff99999fffee1ff111ee0011e1e001cee1e9eeee11
;; 112:f11eeee0ff111eecffff11eefffff1eefffff1eefffff1eefffff1eefffff111
;; 113:e0eeee11eeeee11feee11fffeee1ffffcee1ffffce11ffffee11ffff1111ffff
;; 114:f1eeeee0f11eeeecff1111eefffff1eefffff1eefffff1eefffff1eefffff111
;; 115:e0eee11feeee1fffeee1ffffeee1ffffcee1ffffce11ffffee11ffff1111ffff
;; 116:f11cccc0ff111cccffff11ccfffff1ccfffff1ccfffff1ccfffff1ccfffff111
;; 117:c0cccc11ccccc11fccc11fffccc1ffffccc1ffffcc11ffffcc11ffff1111ffff
;; 118:f1eeeee9f11eeeecff1111eefffff1eefffff1eefffff1eefffff1eefffff111
;; 119:e9eee11feeee1fffeee1ffffeee1ffffcee1ffffce11ffffee11ffff1111ffff
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

(fn test []
  (let [k 0]
    (var okay 0)
    (var continue true)
    (while continue
      (print "ok")
      (set okay 10)
      (set continue false))
    (print okay))) 

(test) 
