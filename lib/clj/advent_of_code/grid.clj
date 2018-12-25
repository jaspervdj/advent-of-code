(ns advent-of-code.grid)

(defrecord Pos [x y])

(defn abs [{x :x y :y}] (+ (Math/abs x) (Math/abs y)))

(defn move [pos dir] (case dir
                      :u (update pos :y dec)
                      :r (update pos :x inc)
                      :d (update pos :y inc)
                      :l (update pos :x dec)))

(defn turn-left [dir] (dir {:u :l :r :u :d :r :l :d}))

(defn turn-right [dir] (dir {:u :r :r :d :d :l :l :u}))

(defn turn [dir lr] (if (= lr :l) (turn-left dir) (turn-right dir)))
