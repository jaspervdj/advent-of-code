(defrecord Disc [id positions start])

(defn parse-disks
  [input]
  (let [pat #"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)."]
    (for [line (clojure.string/split-lines input)
         :let [[_ id positions start] (re-find pat line)]]
      (Disc. (Integer. id) (Integer. positions) (Integer. start)))))

(defn guesses
  [disc]
  (let [t0 (mod (- (:positions disc) (:start disc)) (:positions disc))]
    (filter #(>= % 0)
     (iterate #(+ % (:positions disc)) (- t0 (:id disc))))))

(defn intersection
  "Find the elements common in the sorted (possibly infinite lists)."
  [lists]
  (when-not (some empty? lists)
    (lazy-seq
     (let [min-head (apply min (map first lists))
           max-head (apply max (map first lists))]
       (if
        (every? #(= % min-head) (map first lists))
        (cons min-head (intersection (map rest lists)))
        (intersection (map (fn [l] (drop-while #(< % max-head) l)) lists)))))))

(defn main
  []
  (let [discs (parse-disks (slurp *in*))
        more-discs (cons (Disc. (+ (count discs) 1) 11 0) discs)]
    (println (first (intersection (map guesses discs))))
    (println (first (intersection (map guesses more-discs))))))

(main)
