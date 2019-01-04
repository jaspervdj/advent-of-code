(require 'advent-of-code.md5)
(require 'advent-of-code.grid)
(require 'advent-of-code.dijkstra)
(use '[advent-of-code.grid :only (->Pos)])

(defn open?  [c] (and (>= (int c) (int \b)) (<= (int c) (int \f))))

(defn doors
  [digest]
  (let [open (map open? (take 4 (advent-of-code.md5/md5 digest)))]
    (map first (filter second (map vector [:u :d :l :r] open)))))

(defn neighbours
  [[pos digest]]
  (for [dir (doors digest)
        :let [chr (clojure.string/upper-case (subs (str dir) 1))
              npos (advent-of-code.grid/move pos dir)]
        :when (and (>= (:x npos) 0) (< (:x npos) 4))
        :when (and (>= (:y npos) 0) (< (:y npos) 4))]
    [1 [npos (str digest chr)]]))

(defn distances-to-vault
  [digest]
  (let [goal? (fn [[pos _]] (= (->Pos 3 3) pos))
        distances (advent-of-code.dijkstra/dijkstra
                   [(->Pos 0 0) digest]
                   (fn [pos] (if (goal? pos) [] (neighbours pos))))]
     (map
      (fn [[[_ digest] dist]] [digest dist])
      (filter (fn [[k _]] (goal? k)) distances))))

(defn main
  []
  (let [digest (clojure.string/trim (slurp *in*))
        strip-digest #(subs % (count digest))
        distances (sort-by second (distances-to-vault digest))]
    (println (strip-digest (first (first distances))))
    (println (second (last distances)))))

(main)
