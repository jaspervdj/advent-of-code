(ns advent-of-code.grid)

(defrecord Pos [x y])

(def zero (Pos. 0 0))

(defn abs [{x :x y :y}] (+ (Math/abs x) (Math/abs y)))

(defn neighbours
  [{x :x y :y}]
  [(Pos. x (- y 1))
   (Pos. (+ x 1) y)
   (Pos. x (+ y 1))
   (Pos. (- x 1) y)])

(defn move [pos dir] (case dir
                      :u (update pos :y dec)
                      :r (update pos :x inc)
                      :d (update pos :y inc)
                      :l (update pos :x dec)))

(defn turn-left [dir] (dir {:u :l :r :u :d :r :l :d}))

(defn turn-right [dir] (dir {:u :r :r :d :d :l :l :u}))

(defn turn [dir lr] (if (= lr :l) (turn-left dir) (turn-right dir)))

(defn print-grid
  [stream grid]
  (let [min-x (apply min (for [{x :x} (keys grid)] x))
        min-y (apply min (for [{y :y} (keys grid)] y))
        max-x (apply max (for [{x :x} (keys grid)] x))
        max-y (apply max (for [{y :y} (keys grid)] y))]
    (doseq [y (range min-y (+ max-y 1))]
      (.println
       stream
       (apply str (for [x (range min-x (+ max-x 1))]
                    (or (get grid (Pos. x y)) \space)))))))

(defn parse-grid
  [input]
  (into {} (for [[y line] (map-indexed
                   (fn [idx line] [idx line])
                   (clojure.string/split-lines input))
                 [x c] (map-indexed (fn [idx c] [idx c]) line)]
             [(->Pos x y) c])))
