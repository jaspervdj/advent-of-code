(defn parse-token [c] (case c \( 1 \) -1))
(defn part1 [input] (reduce + 0 (map parse-token input)))
(defn part2
  [input]
  (reduce
   (fn [acc [i x]] (if (< (+ acc x) 0) (reduced (+ i 1)) (+ acc x)))
   0
   (map-indexed (fn [i x] [i (parse-token x)]) input)))

(defn main
  []
  (let [input (clojure.string/trim (slurp *in*))]
    (println (part1 input))
    (println (part2 input))))

(main)
