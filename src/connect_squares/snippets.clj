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

(defn get-midpoints [origin size]
  (let [[x y] origin]
   {:top [(+ x (/ size 2)) y]
    :left [x (+ y (/ size 2))]
    :bottom [(+ x (/ size 2)) (+ y size)]
    :right [(+ x size) (+ y (/ size 2))]
    }))