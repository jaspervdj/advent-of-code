;; I was about to figure out if there is a cycle/pattern in the input for part 2
;; but it turns out clojure is fast enough and we can just brute force it.

(defn evolve
  [l c r]
  (not (or
        (and (not l) (not c) r)
        (and l (not c) (not r))
        (and (not l) c r)
        (and l c (not r)))))

(defn next-row
  [row]
  (vec (map-indexed
        (fn [i v] (evolve
                   (if (> i 0) (nth row (- i 1)) true)
                   v
                   (if (< (+ i 1) (count row)) (nth row (+ i 1)) true)))
        row)))

(defn string-to-row [row] (vec (for [c row] (= c \.))))
(defn row-to-string [row] (apply str (map #(if % \. \^) row)))

(defn safe-tiles
  [initial-row rows]
  (->> initial-row
       (iterate next-row)
       (take rows)
       (apply concat)
       (filter true?)
       count))

(defn main
  []
  (let [initial-row (string-to-row (clojure.string/trim (slurp *in*)))]
    (println (safe-tiles initial-row 30))
    (println (safe-tiles initial-row 400000))))

(main)
