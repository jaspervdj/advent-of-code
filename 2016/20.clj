(defrecord Range [lo hi])

(defn range-contains? [r x] (and (>= x (:lo r)) (<= x (:hi r))))
(defn range-join [l r] (Range. (min (:lo l) (:lo r)) (max (:hi l) (:hi r))))
(defn range-size [r] (- (:hi r) (:lo r) -1))

(defn range-overlaps?
  [l r]
  (or (range-contains? l (:lo r)) (range-contains? l (:hi r))
      (range-contains? r (:lo l)) (range-contains? r (:hi l))))

(defn range-parse
  [line]
  (let [[_ lo hi] (re-find #"^(\d+)-(\d+)$" line)]
    (Range. (Long. lo) (Long. hi))))

(defn parse-ranges [input] (map range-parse (clojure.string/split-lines input)))

(defn join-overlapping-ranges
  [initial-ranges]
  (loop [ranges initial-ranges
         size (count initial-ranges)
         acc '()]
    (if-let [r (first ranges)]
      ;; Take all ranges that overlap and throw them together.
      (let [xs (filter #(range-overlaps? r %) (rest ranges))
            ys (remove #(range-overlaps? r %) (rest ranges))]
        (recur ys size (cons (reduce range-join r xs) acc)))
      ;; Did anything change?  If so go through the whole thing again.
      (if (= size (count acc)) acc (recur acc (count acc) '())))))

(defn blacklist?  [ranges port] (some (fn [r] (range-contains? r port)) ranges))

(defn lowest-allowed
  [ranges]
  (let [candidates (sort (conj (map inc (map :hi ranges)) 0))]
    (first (remove #(blacklist? ranges %) candidates))))

(defn main
  []
  (let [ranges (join-overlapping-ranges (parse-ranges (slurp *in*)))]
    (println (lowest-allowed ranges))
    (println (- 4294967295 (reduce + (map range-size ranges)) -1))))

(main)
