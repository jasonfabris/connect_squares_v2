(ns connect_squares.core2
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.matrix :as mx]))


(def W 1000)
(def H 1000)
(def grid-size-x 28) ;; number of cells wide
(def grid-size-y 8) ;; number of cells high
(def prop-area 0.57)  ;; how much of the cell does the box fill?

(comment
  (def x1 [0 1 2 3 4])
  (def x2 [0 1 2 3 4])
  (def xy (for [x x1 y x2] (vector x y)))  ;; cartesian product
  (map (partial * 300) [0 1])
  )


(defn- scale-value
  "Linearly transforms x from range input-range to output-range where:
   input-range - a vector like [min max]
   output-range - a vector like [min max]
   "
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


(defn make-grid [grid-x grid-y]
  "make grid matrix of indices - grid-x and y are number of cells"
  (for [x (range grid-x) y (range grid-y)] (vector x y)))

;;cell-width (/ W grid-size-x)
;;cell-height (/ H grid-size-y)
;;xy (for [x (range grid-size-x) y (range grid-size-y)] (vector x y))


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

(defn get-midpoints [origin size]
  (let [[x y] origin]
   {:top [(+ x (/ size 2)) y]
    :left [x (+ y (/ size 2))]
    :bottom [(+ x (/ size 2)) (+ y size)]
    :right [(+ x size) (+ y (/ size 2))]
    }))

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
      :bottom (if (< y grid-size-y) 
                (mx/add cell-idx [0 1])
                nil)
      :left (if (> x 0)
              (mx/add cell-idx [-1 0])
              nil)
      :right (if (< x grid-size-x)
               (mx/add cell-idx [1 0])
               nil))))

(defn build-inner-shp
  "create the inner shape"
  [grid-idx]
  (let [cell-width (/ W grid-size-x)
        cell-height (/ H grid-size-y)
        adj (inner-orig-adj cell-width prop-area)
        origin (mx/add grid-idx adj)
        width-inner (- cell-width (* 2 adj))
        height-inner (- cell-height (* 2 adj))
        [xo yo] origin
        x-wob (scale-value (rand) [0 1] [(- (/ width-inner 2)) (/ width-inner 2)])]
    (q/begin-shape)
    (q/vertex xo yo)
    (q/vertex (+ xo width-inner x-wob) yo)
    (q/vertex (+ xo width-inner) (+ yo height-inner))
    (q/vertex xo (+ yo height-inner))
    (q/end-shape :close)
    ;;change to return map with vertices, midpoints, neighbour?
    ;;sep func to draw it
    ))


(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb 360 100 100 1)
  (q/background 240)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :color2 180
   :grid (make-grid grid-size-x grid-size-y)})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.6) 360)
   :color2 (mod (+ (:color state) 180.7) 360)})

(defn draw-state [state]
  ;;(q/no-loop)
  ;;(q/print-first-n 5 (:grid state))
  ;;(q/print-first-n (:color state) (:color2 state))
  ;;(println (:color state) (:color2 state))
  (q/fill (:color state) 100 100)
  (q/frame-rate 12)
  ;;(q/no-stroke)
  
  (q/background (:color state) 100 100 0.3)
  
  (let [cell-width (/ W grid-size-x)
        cell-height (/ H grid-size-y)
        xy (for [x (range grid-size-x) y (range grid-size-y)] (vector x y))
        ;;origins (mx/scale xy cell-width)
        origins (map cell-idx-to-xy (make-grid grid-size-x grid-size-y))
        adj (inner-orig-adj cell-width prop-area)
        origins-inner (mx/add origins adj)
        width-inner (- cell-width (* 2 adj))]  ;; cartesian product
    
    ;;outer squares
    ; (doseq [o origins] 
    ;   (draw-rect o cell-width))
    
    ;;inner squares
   ;; (doseq [o origins-inner]
   ;;   (q/fill (:color2 state) 100 100 0.75)
   ;;   (draw-rect o width-inner))
    
    (doseq [o origins]
      (if (> (rand) 0.3)
        (do
          (q/fill (:color2 state) 100 100 0.75)
          (q/no-stroke))
        (do
          (q/stroke (:color2 state) 100 100 0.75)
          (q/no-fill)))
      (build-inner-shp o))
    
    ;;connectors
    (doseq [o origins-inner]
      (let [mids (get-midpoints o width-inner)
            side (rand-nth [:top :bottom :left :right])
            [x y] (side mids)
            neighb-orig (mx/add o (mx/scale (neighbour-origin side) cell-width))
            neighb-mids (get-midpoints neighb-orig width-inner)
            n-side (case side
                     :top :bottom
                     :bottom :top
                     :left :right
                     :right :left)
            [x2 y2] (n-side neighb-mids)
            ]
        ;;(q/stroke 0 0 0)
        ;;(q/rect (first neighb-orig) (second neighb-orig) 30 30)
        ;;(println "orig:" o "n-orig:" neighb-orig side n-side mids neighb-mids x y x2 y2)
        ;;(q/ellipse x y 20 20)
        (q/stroke (:color2 state) 100 100 0.6)
        (q/stroke-weight 3.5)
        ;;(q/no-fill)
        ;;(q/ellipse x y 30 30)
        (if (> (rand) 0.6)
          (q/line [x y] [x2 y2]))
        (q/no-stroke)
        ;;(q/line (first (:top mids)) (second (:top mids)) 20 20)
        ;;(q/line (first (:left mids)) (second (:left mids)) 20 20))
        )
      ))
  (if (q/key-pressed?)
    (q/save "connected-rects.png")))


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
