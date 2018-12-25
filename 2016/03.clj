(defn triangle? [specs] (let [[x y z] (sort specs)] (> (+ x y) z)))

(defn parse-specifications
  [input]
  (vec (map
        (fn [line] (vec (map
                         #(Integer. %)
                         (remove empty? (clojure.string/split line #"\s+")))))
        (clojure.string/split-lines input))))

(defn columns
  [specs]
  (for [row (range 0 (count specs) 3)
        col (range 0 3)]
    (vec (for [i (range 0 3)] (nth (nth specs (+ row i)) col)))))

(defn main
  []
  (let [specifications (parse-specifications (slurp *in*))]
    (println (count (filter triangle? specifications)))
    (println (count (filter triangle? (columns specifications))))))

(main)
