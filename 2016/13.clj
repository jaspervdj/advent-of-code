(use 'advent-of-code.grid)
(require 'advent-of-code.dijkstra)

(defn wall?
  [{x :x y :y} favorite-number]
  (let [curve (+ (* x x) (* 3 x) (* 2 x y) y (* y y))]
    (odd? (Integer/bitCount (+ curve favorite-number)))))

(defn distances
  [w h favorite-number]
  (advent-of-code.dijkstra/dijkstra
   (->Pos 1 1)
   (fn [pos]
     (for [nb (neighbours pos)
           :when (and (>= (:x nb) 0) (< (:x nb) w))
           :when (and (>= (:y nb) 0) (< (:y nb) h))
           :when (not (wall? nb favorite-number))]
       [1 nb]))))

(defn main
  []
  (let [fav-number (Integer. (slurp *in*))
        dists (distances 50 50 fav-number)]
    (println (get dists (->Pos 31 39)))
    (println (count (filter #(<= % 50) (vals dists))))))

(main)
