;; Rather than computing the dragon curve as a string, we provide a function
;; that gives you the right bit for any index.  That way we never need to store
;; anything more than the original pattern in memory.
;;
;; I'm sure there are ways to fuse the checksum and curve generation even more,
;; but this is nicely decomposed and runs in reasonable time.

(defn dragon-curve
  [pattern size i]
  (let [half (quot (- size 1) 2)]
    (cond
      (< i (count pattern)) (nth pattern i)
      (= i half) false
      (< i half) (dragon-curve pattern half i)
      (> i half) (not (dragon-curve pattern half (- size 1 i))))))

(defn dragon-curve-size
  [pattern i]
  (first (filter #(<= i %) (iterate #(+ 1 % %) (count pattern)))))

(defn checksum
  [coll len]
  (if
   (odd? len)
   coll
   (checksum (map #(= (first %) (second %)) (partition 2 coll)) (quot len 2))))

(defn dragon-curve-checksum
  [pattern len]
  (let [size (dragon-curve-size pattern len)]
    (checksum (for [i (range 0 len)] (dragon-curve pattern size i)) len)))

(defn parse-bitstring [string] (vec (for [c string] (= \1 c))))
(defn to-bitstring [bools] (apply str (map #(if % \1 \0) bools)))

(defn main
  []
  (let [pattern (parse-bitstring (clojure.string/trim (slurp *in*)))]
    (println (to-bitstring (dragon-curve-checksum pattern 272)))
    (println (to-bitstring (dragon-curve-checksum pattern 35651584)))))

(main)
