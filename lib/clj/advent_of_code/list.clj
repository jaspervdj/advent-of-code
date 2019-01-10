(ns advent-of-code.list)

(defn in?
  "Check if coll contains x."
  [coll x] (some #(= x %) coll))

(defn max-by
  "Like max-key but uses compare rather than a numeric key."
  [keyfn coll]
  (when-not (empty? coll)
   (first
    (reduce
     (fn [[x xk] y]
       (let [yk (keyfn y)] (if (> (compare xk yk) 0) [x xk] [y yk])))
     [(first coll) (keyfn (first coll))]
     (rest coll)))))

(defn twice
  "Return the first element in the sequence that occus twice."
  [coll]
  (first
   (reduce
    (fn [[_ s] x] (if (contains? s x) (reduced [x s]) [nil (conj s x)]))
    [nil #{}]
    coll)))

(defn permutations
  [s]
  (lazy-seq
   (if
    (seq (rest s))
    (apply concat (for [x s]
                    (map #(cons x %) (permutations (remove #{x} s)))))
    [s])))
