(ns connect_squares.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.matrix :as mx]
            [clojure.pprint :as pp]))

(def aspect-ratio 4/6)  ;; make < 1 if you have more y cells than x, > 1 otherwise

(def H 1400)
(def W (* H aspect-ratio))

(def grid-size-x 8) ;; number of cells wide
(def grid-size-y 12 #_(* (/ W H) grid-size-x)) ;; number of cells high

;(def prop-area 0.941)  ;; how much of the cell does the box fill?
;(def inner-lvls 13)ss
(def prop-area 0.19734) ;;redundant  ;; how much of the cell does the box fill?
(def shrink-props [0.92 0.7 0.65 0.22 0.12] #_[0.9 0.34 0.22 0.3] #_[0.734 0.6 0.5 0.4 0.3])
(def inner-lvls (count shrink-props))

(def connector-factor 0.19) ;; how random the length of the connector is
(def connector-chance 0.79) ;; how likely a shape is to have a connector
(def connector-lvl 1) ;; the level the connectors are drawn between
(def for-plotter true)
(def fill-chance 1)  ;;0.78 prob the inner-sqaure stays unfilled (bad name - fix)

;; (defn key-press [k]
  ;; (let [dte (str (java.time.LocalDateTime/now))
        ;; dir "C:/Users/Jason/OneDrive/Art_Output/"
        ;; fname (str dir "ConnectedSquares_v2_" dte ".png")]
    ;; (println fname)
    ;; (q/save-frame fname)))

(defn flat-idx [idx rows cols]
  (let [[x y] idx
        m (* x (- cols 1))
        ]
    #_(println "X: " x " Y: " y   "M: " m " Idx: "
             (+ m  (+ x y)))
    (+ m  (+ x y))
    ))

(comment 
  (def tstgrid (make-grid 2 5))
  (map #(flat-idx %1 2 5) tstgrid)

(defn flat-idx [idx rows cols]
  (let [[x y] idx]
    (->> (* x rows)
         (+ y x))))  
)

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
  [idx x-size y-size]
  (let [cell-width (/ W x-size)
        cell-height (/ H y-size)]
    (mx/mul idx [cell-width cell-height])))

(defn draw-rect
      "translate then draw rect"
      [origin size]
      (let [[x y] origin]
        (q/with-translation [x y]
          (q/rect 0 0 size size)))
             #_(q/fill 306 50 10 0.8))

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
       {:idx [-1 -1]
        :tl tl :tr tr :br br :bl bl
        :mid-t (midpoint tl tr)
        :mid-r (midpoint tr br)
        :mid-b (midpoint bl br)
        :mid-l (midpoint tl bl)
        :lvl lvl}))))

;;TODO - cell-height makes rectangles if not equal to cell-width
;;       BUT, if you force it to squares, the rows appear at the top of each background cell and look
;;       dumb. (try 20 cols and 2 rows, for example.)
(defn make-inner-shapes 
  "return the shapes for all inner shapes based on levels"
  [grid-cells-x grid-cells-y num-lvls shrink-prop]
  (let [g (make-grid grid-cells-x grid-cells-y)
        cell-width (/ W grid-size-x)
        cell-height cell-width #_(/ H grid-size-y)
        r (map inc (range num-lvls))
        ;; r (range (num-lvls))
        ;; adj (into [] (map #(apply * (vec (repeat %1 shrink-prop))) r))
        adj shrink-props
        shps (for [a adj]
          (let [w (* cell-width a)
                h (* cell-height a)
                l (.indexOf adj a)
                orig (->> g
                          (map #(cell-idx-to-xy %1 grid-size-x grid-size-y)) 
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
  (let [[x0 y0] (:tl shp) [x1 y1] (:tr shp)
        [x2 y2] (:br shp) [x3 y3] (:bl shp)]
    ;;(println "V: " x0 y0 x1 y1 x2 y2 x3 y3)
    (q/begin-shape)
    (q/vertex x0 y0)
    (q/vertex x1 y1)
    (q/vertex x2 y2)
    (q/vertex x3 y3)
    (q/end-shape :close)))

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
                   y (range grid-size-y)] (vector x y))
          init-col (rand 360)]
    {:color init-col 
     :color2 (+ 180 init-col)
     :grid grid
     :origins (map #(cell-idx-to-xy %1 grid-size-x grid-size-y) grid)
     :origins-inner (mx/add (map #(cell-idx-to-xy %1 grid-size-x grid-size-y) grid) adj)
     :origins-inner2 (mx/add (map #(cell-idx-to-xy %1 grid-size-x grid-size-y) grid) adj2)
     :cell-width cell-width
     :cell-height cell-height
     :width-inner width-inner
     :height-inner height-inner
     :width-inner2 width-inner2
     :height-inner2 height-inner2
     :new-shp-grid (shape-maker 0 0)
     :new-shp (shape-maker x-wob y-wob)
     :inner-shps (map #((shape-maker x-wob y-wob) %1
                                                   width-inner
                                                   height-inner)
                      (mx/add (map 
                               #(cell-idx-to-xy %1 grid-size-x grid-size-y) grid) 
                              adj))
     :inner-shps2 (map #((shape-maker x-wob y-wob) %1
                         width-inner2
                         height-inner2)
                       (mx/add (map #(cell-idx-to-xy %1 grid-size-x grid-size-y) grid) 
                               adj2))
     :inners (make-inner-shapes grid-size-x grid-size-y inner-lvls prop-area)}))

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


  (defn draw-state [state]
    ;;(q/no-loop)
    ;;(q/print-first-n 1 (last (:origins-inner state)))
    ;;(println (:inners state))
        
    (q/frame-rate 1)
    ;;(q/background (:color state) 100 100 0.3)
     
    ;;outer squares
    ;; if the plotter flag is set, plain white background, otherwise grid of squares
    ;; those are currently all the same colour, but you can change that!
    (if-not for-plotter
      (doseq [o (:origins state)]
        (let [w (:cell-width state)
              h (:cell-height state)
              s ((:new-shp-grid state) o w h)]
          (q/no-stroke)
          (q/fill (:color state) 100 100 1.0)
          ;;(q/fill (mod (+ (:color state) 
          ;; (.indexOf (:origins state) o)) 360) 100 100)
          (draw-shp s)))
      (q/background 200 0 100))

    ;;(println "Count: " (count (:inners state)))

    ;; very wide strokes
    ;; alpha increasing with lvl
    ;; alternating cols by lvl

    ;;inner squares
    (doseq [s (:inners state)]
      (if (> (rand) fill-chance) ;; outline only if no fill chance
        (do
          (q/fill (:color2 state) 100 100 0.75)
          (q/no-stroke))
        (do
          (let [i (/ 360 (+ 1 inner-lvls))
                adj (+ 30 (* i (:lvl s)))
                h (mod (+ adj (:color2 state)) 360)]
            (q/stroke-weight 2)
            (q/stroke h 100 100 0.75) ;; alpha 0.75
            (q/no-fill))))
      (draw-shp s))
    ;;(q/fill (mod (+ (:color2 state) (.indexOf (:origins state) s)) 360) 100 100)

    (comment
      (defn make-connectors 
        "make connectors between shapes"
        [shps lvl]))
    
    ;;connectors
    ;;pick a side on current shape
    ;;find the neighbour
    ;;get the proper midpoint
    (let [shps (filter #(= (:lvl %1) connector-lvl) (:inners state))
          midpts [:mid-t :mid-r :mid-b :mid-l]
          sides [:top :right :bottom :left]]
      (doseq [s shps]
        (let [idx (.indexOf shps s)
              side-idx (rand-int 4)
              side (nth sides side-idx)
              n-idx (find-neighbour-idx side (nth (:grid state) idx))
              midpt-side (nth midpts side-idx)
              midpt (get s midpt-side)
              n-mid (nth (cycle midpts) (+ side-idx 2))
              n-shp (if (nil? n-idx)
                      false
                      (do 
                        #_(println n-idx (count shps) 
                                 (.indexOf shps n-idx)
                                 (flat-idx n-idx grid-size-x grid-size-y))
                        (nth shps (flat-idx n-idx grid-size-x grid-size-y)))
                      )
              n-midpt (if (nil? n-idx)
                        false
                        (get n-shp n-mid))]
          (if (and (< (rand) connector-chance) (not (nil? n-idx)))
            (let [[x1 y1] midpt
                  [x2 y2] n-midpt
                  v (mx/sub midpt n-midpt)
                  mag (mx/magnitude v)
                  adj (* connector-factor mag)]
              (q/stroke-weight 3.75)
              (q/stroke (:color2 state) 100 100 0.6) ;; alpha 0.6
              ;;(q/no-fill) ;;(q/ellipse x1 y1 25 25)
              ;;(q/fill 360 100 0 1) ;;(q/ellipse x2 y2 25 25)
              (q/line (+ x1 (- (rand adj) (/ adj 2))) 
                      (+ y1 (- (rand adj) (/ adj 2)))
                      (+ x2 (- (rand adj) (/ adj 2)))
                      (+ y2 (- (rand adj) (/ adj 2))))))))))

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

  (q/defsketch connect_squares
    :title "Connected Squares"
    :size [W H]
    :setup setup
    :update update-state
    :draw draw-state 
    :mouse-clicked (fn [state _]
                     (let [dt (.format (java.text.SimpleDateFormat. "yyyy-MM-dd-hhmmss") 
                                       (new java.util.Date))
                           nm (str "C:/Users/Jason/OneDrive/Art_Output/" "connected_squares_" dt)]
                       (q/save (str nm ".png"))
                       (q/do-record (q/create-graphics W H :pdf (str nm ".pdf"))
                                    (q/color-mode :hsb 360 100 100 1)
                                    (draw-state state))
                       state))
    :features [:keep-on-top]
    :middleware [m/fun-mode])

;;(defn -main [& args])



