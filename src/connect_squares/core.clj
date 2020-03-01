(ns connect_squares.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.matrix :as mx]
            [clojure.pprint :as pp]))


(def W 1400)
(def H 1400)
(def grid-size-x 11) ;; number of cells wide
(def grid-size-y (* (/ W H) grid-size-x)) ;; number of cells high
;(def prop-area 0.941)  ;; how much of the cell does the box fill?
;(def inner-lvls 13)
(def prop-area 0.34)  ;; how much of the cell does the box fill?
(def inner-lvls 3)

(comment
  (def x1 [0 1 2 3 4])
  (def x2 [0 1 2 3 4])
  (def xy (for [x x1 y x2] (vector x y)))  ;; cartesian product
  (map (partial * 300) [0 1])
  )


(defn flat-idx [idx]
  (let [[x y] idx]
    (->> (* x grid-size-x)
         (+ y))))

(defn midpoint [p1 p2]
  (let [[x1 y1] p1
       [x2 y2] p2]
  [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)]))


(defn- scale-value
  "Linearly transforms x from range input-range to output-range where:
   input-range - a vector like [min max]
   output-range - a vector like [min max]"
  [x input-range output-range]
  (let [[a b] input-range
        [c d] output-range]
    (+
     (-> (- x a)
         (/ (- b a))
         - ; negate the result
         inc
         (* c))
     (-> (- x a)
         (/ (- b a))
         (* d)))))

(defn make-grid 
  "make grid matrix of indices - grid-x and y are number of cells"
  [grid-x grid-y]
  (for [x (range grid-x) y (range grid-y)] (vector x y)))

;;;TODO REPLACE THE LET PART IN DRAW STATE WITH THIS
;; make the inner squares more random
;; find the midpoints of the inner new shapes
;; make lines to connect shapes (new midpoints)
;; make the connections less random
;; prevent connections out of frame (edge rows)

(defn cell-idx-to-xy 
  "grid index (x y) to scaled xy origin"
  [idx]
  (let [cell-width (/ W grid-size-x)
        cell-height (/ H grid-size-y)]
    (mx/mul idx [cell-width cell-height])))

 (defn draw-rect
      "translate then draw rect"
      [origin size]
      (let [[x y] origin]
        (q/with-translation [x y]
          (q/rect 0 0 size size)))
             ;(q/fill 306 50 10 0.8)
             ;(q/rect 0 0 rect-width rect-height)))  
      )
(defn inner-orig-adj
  "how much inside the cell should the inner square origin be?"
  [size prop]
  (let [sz (- size (* size prop))
        adj (/ sz 2)]
    (+ adj (rand-int 5))))

(defn neighbour-origin 
  "find the origin point of the neighbour in dir
   so we can find the proper midpoint and draw a line to it"
  [dir]
  (let [[h-off v-off] (case dir
                        :top [0 -1]
                        :bottom [0 1]
                        :right [1 0]
                        :left [-1 0]
                        nil)]
    [h-off v-off]
    ))

(defn find-neighbour-idx 
  "given a direction, what's the index of the grid for that neighbour?
  this doesn't check if the idx is valid for the grid - should it?"
  [dir cell-idx]
  (let [[x y] cell-idx]
    (case dir
      :top (if (> y 0) 
             (mx/add cell-idx [0 -1])
             nil)
      :bottom (if (< y (- grid-size-y 1)) 
                (mx/add cell-idx [0 1])
                nil)
      :left (if (> x 0)
              (mx/add cell-idx [-1 0])
              nil)
      :right (if (< x (- grid-size-x 1))
               (mx/add cell-idx [1 0])
               nil))))

(comment
  (let [grid (make-grid 4 4)
        idx (find-neighbour-idx :top [1 1])
        pos (.indexOf grid idx)]
    (println grid idx pos))
  )

(defn foo
  ([a]
   (foo a nil))
  ([a b]
   (println "Required argument a is" a)
   (println "Optional argument b is" b)))

;;TODO - probably either both w/h and rand should be outside or inside
;; inner fn. Weird that one is out, one is in
(defn shape-maker 
  "returns a function that takes origin, width, height 
   and builds a shape from it"
  ([x-rnd y-rnd]
   (shape-maker x-rnd y-rnd 1))
  ([x-rnd y-rnd lvl]
   (fn [origin w h]
     (let [[xo yo] origin
           x-wob (fn [] (scale-value (rand) [0 1] 
                                     [(- x-rnd) x-rnd])) 
           y-wob (fn [] (scale-value (rand) [0 1]
                                     [(- y-rnd) y-rnd]))
           tl [(+ xo (x-wob)) (+ yo (y-wob))]
           tr [(+ xo w (x-wob)) yo]
           br [(+ xo w (x-wob)) (+ yo h (y-wob))]
           bl [(+ xo) (+ yo h)]]
       {:tl tl :tr tr :br br :bl bl
        :mid-t (midpoint tl tr)
        :mid-r (midpoint tr br)
        :mid-b (midpoint bl br)
        :mid-l (midpoint tl bl)
        :lvl lvl}))))


(defn make-inner-shapes 
  "return the shapes for all inner shapes based on levels"
  [grid-cells-x grid-cells-y num-lvls shrink-prop]
  (let [g (make-grid grid-cells-x grid-cells-y)
        cell-width (/ W grid-size-x)
        cell-height (/ H grid-size-y)
        adj (into [] (map #(apply * (vec (repeat %1 shrink-prop))) (map inc (range num-lvls))))
        shps (for [a adj]
          (let [w (* cell-width a)
                h (* cell-height a)
                l (.indexOf adj a)
                orig (->> g
                          (map cell-idx-to-xy) 
                          (mx/add  [(inner-orig-adj cell-width a) (inner-orig-adj cell-height a)]))
                x-wob (scale-value (rand) [0 1]
                                   [(- (/ w 5))
                                    (/ w 5)])
                y-wob (scale-value (rand) [0 1]
                                   [(- (/ h 5))
                                    (/ h 5)])]
            ;; (println a " : " l)
            (mapv #((shape-maker x-wob y-wob l) %1 w h) orig)))]
    (flatten shps)))

(comment
   (->> (make-grid 2 2) 
        (map cell-idx-to-xy)
        (mx/add  (mx/mul 0.6))
        (#(mx/div %1 2)))

  (into [] (map #(apply * (vec (repeat %1 0.5))) (range 3)))
  (def tst (make-inner-shapes 2 2 2 0.5))
  (defn rs [lim] 
    (scale-value (rand) [0 1]
                 [(- lim) lim]))
  (rs 5)
  (let [new-shp (shape-maker 5 5)]
    (new-shp [150 200] 10 10))
  (make-inner-shapes 2 2 2 0.5)  
  ((shape-maker 5 5) [5 5] 40 40))

(defn draw-shp 
  "takes shape map and draws the vertices"
  [shp]
  (let [[x0 y0] (:tl shp)
        [x1 y1] (:tr shp)
        [x2 y2] (:br shp)
        [x3 y3] (:bl shp)]
    ;;(println "V: " x0 y0 x1 y1 x2 y2 x3 y3)
    (q/begin-shape)
    (q/vertex x0 y0)
    (q/vertex x1 y1)
    (q/vertex x2 y2)
    (q/vertex x3 y3)
    (q/end-shape :close)))

;; (defn save_pdf []
    ;; (q/do-record (q/create-graphics 1400 1400 :pdf "con_squares.pdf")
                       ;; (draw-state))
                       ;; (q/exit))


 (defn setup []
    (q/frame-rate 3)
    (q/color-mode :hsb 360 100 100 1)
    
    (let [cell-width (/ W grid-size-x)
          cell-height (/ H grid-size-y)
          adj (inner-orig-adj cell-width prop-area)
          adj2 (inner-orig-adj cell-width (* prop-area 0.85)) ;;CHG HARD CODE
          width-inner (- cell-width (* 2 adj))
          height-inner (- cell-height (* 2 adj))
          width-inner2 (- cell-width (* 2 adj2))
          height-inner2 (- cell-height (* 2 adj2))
          x-wob (scale-value (rand) [0 1]
                             [(- (/ width-inner 4))
                              (/ width-inner 4)])
          y-wob (scale-value (rand) [0 1]
                             [(- (/ height-inner 4))
                              (/ height-inner 4)])
          grid (make-grid grid-size-x grid-size-y)
          xy (for [x (range grid-size-x)
                   y (range grid-size-y)] (vector x y))]
    {:color 0
     :color2 180
     :grid grid
     :origins (map cell-idx-to-xy grid)
     :origins-inner (mx/add (map cell-idx-to-xy grid) adj)
     :origins-inner2 (mx/add (map cell-idx-to-xy grid) adj2)
     :cell-width cell-width
     :cell-height cell-height
     :width-inner width-inner
     :height-inner height-inner
     :width-inner2 width-inner2
     :height-inner2 height-inner2
     :new-shp-grid (shape-maker 0 0)
     :new-shp (shape-maker x-wob y-wob)
     :inner-shps (map  #((shape-maker x-wob y-wob) %1
                                                   width-inner
                                                   height-inner)
                       (mx/add (map cell-idx-to-xy grid) adj))
     :inner-shps2 (map  #((shape-maker x-wob y-wob) %1
                                                   width-inner2
                                                   height-inner2)
                       (mx/add (map cell-idx-to-xy grid) adj2))
     :inners (make-inner-shapes grid-size-x grid-size-y inner-lvls prop-area)}))

(comment
  (for [n (range inner-lvls)]
    (doseq [o (:origins state)]
      (let [w (:cell-width state)
            h (:cell-height state)
            s ((:new-shp-grid state) o w h)
            p (apply * (vec  (repeat n 0.9)))
            adj (inner-orig-adj )]))))

  (defn update-state [state]
     (let [x-wob (scale-value (rand) [0 1]
                              [(- (/ (:width-inner state) 4))
                               (/ (:width-inner state) 4)])
           y-wob (scale-value (rand) [0 1]
                              [(- (/ (:height-inner state) 4))
                               (/ (:height-inner state) 4)])]
       {:grid (:grid state)
        :origins (:origins state)
        :origins-inner (:origins-inner state)
        :origins-inner2 (:origins-inner2 state)
        :cell-width (:cell-width state)
        :cell-height (:cell-height state)
        :width-inner (:width-inner state)
        :height-inner (:height-inner state)
        :width-inner2 (:width-inner2 state)
        :height-inner2 (:height-inner2 state)
        :new-shp-grid (shape-maker 0 0)
        :new-shp (shape-maker x-wob y-wob)
        :color (mod (+ (:color state) 0.6) 360)
        :color2 (mod (+ (:color state) 180.7) 360)
        :inner-shps (map  #((:new-shp state) %1
                            (:width-inner state)
                            (:height-inner state))
                          (:origins-inner state))
        :inner-shps2 (map  #((:new-shp state) %1
                             (:width-inner2 state)
                             (:height-inner2 state))
                           (:origins-inner2 state))
        :inners (make-inner-shapes grid-size-x grid-size-y inner-lvls prop-area)}))

(comment
  (map #(mod (+ %1 0.6) 360) (range 0 380 10))
  (map  #((shape-maker 0 0) %1 15 15) 
        [[0 0] [100 0] [0 200]])
  )

  (defn draw-state [state]
    ;;(q/no-loop)
    ;;(q/print-first-n 1 (last (:origins-inner state)))
   ;;(println (:inners state))

    (q/no-stroke)
    (q/frame-rate 2)
    ;;(q/background (:color state) 100 100 0.3)
    (q/fill (:color state) 100 100)
    ;;outer squares
    (doseq [o (:origins state)]
      (let [w (:cell-width state)
            h (:cell-height state)
            s ((:new-shp-grid state) o w h)]
        ;;(q/fill (mod (+ (:color state) (.indexOf (:origins state) o)) 360) 100 100)
        (draw-shp s)))

    ;;(println "Count: " (count (:inners state)))

;; very wide strokes
;; alpha increasing with lvl
;; alternating cols by lvl

    ;;inner squares
    (doseq [s (:inners state)]
      (if (> (rand) 0.9) ;; outline only if no fill chance
        (do
          (q/fill (:color2 state) 100 100 0.55)
          (q/no-stroke))
        (do
          (q/stroke (* (:lvl s) (:color2 state)) 100 100 0.75)
          (q/no-fill)))
      ;;(q/fill (mod (+ (:color2 state) (.indexOf (:origins state) s)) 360) 100 100)
      (draw-shp s))
 
    ;;connectors
                                        ;pick a side on current shape
    ;;find the neighbour
    ;;get the proper midpoint
    
    (doseq [s (:inner-shps state)]
      (let [midpts [:mid-t :mid-r :mid-b :mid-l]
            sides [:top :right :bottom :left]
            idx (.indexOf (:inner-shps state) s)
            side-idx (rand-int 4)
            side (nth sides side-idx)
            n-idx (find-neighbour-idx side (nth (:grid state) idx))
            midpt-side (nth midpts side-idx)
            midpt (get s midpt-side)
            n-mid (nth (cycle midpts) (+ side-idx 2))
            n-shp (if (nil? n-idx)
                    false
                    (nth (:inner-shps state) (flat-idx n-idx)))
            n-midpt (if (nil? n-idx)
                      false
                      (get n-shp n-mid))]

        ;;(println "N: " idx midpt n-idx n-midpt)
        (if (and (> (rand) 0.3) (not (nil? n-idx)))
          (let [[x1 y1] midpt
                [x2 y2] n-midpt]
            (q/stroke (:color2 state) 100 100 0.6)
            (q/stroke-weight 3.75)
            ;;(q/no-fill)
            ;;(q/ellipse x1 y1 25 25)
            
            ;;(q/fill 360 100 0 1)
            ;;(q/ellipse x2 y2 25 25)
            (q/line x1 y1 x2 y2)))))
    #_(save_pdf))


(comment
  (def midpts [:mid-t :mid-r :mid-b :mid-l])
  (let [grid (make-grid grid-size-x grid-size-y)
        n-idx (find-neighbour-idx :bottom (nth grid 1))
        midpt-side (nth midpts 2)
        midpt (get s midpt-side)
        n-mid (nth (cycle midpts) (+ side-idx 2))])
 
  (let [grid (make-grid grid-size-x grid-size-y)
        sides [:top :right :left :bottom]
        c_s (for [c grid s sides] {:cell c :side s})]
    (doseq [cell c_s] 
      ;;(println (:side cell) (:cell cell))
      (println (:side cell) (:cell cell) (find-neighbour-idx (:side cell) (:cell cell)))))
  )
(comment
  (let [sides [:mid-t :mid-r :mid-b :mid-l]]
    (map #(nth (cycle sides) %1) (range 0 10))
    )
  (find-neighbour-idx :top [0 0])
  )


  (q/defsketch connect_squares
    :title "Connected Squares"
    :size [W H]
  ; setup function called only once, during sketch initialization.
    :setup setup
  ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state 
    :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
    :middleware [m/fun-mode])

;;(defn -main [& args])
