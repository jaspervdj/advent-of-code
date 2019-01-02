(require 'advent-of-code.md5)

(defn sliding-window
  [size coll]
  (lazy-seq
   (when-not (empty? coll)
     (cons (take size coll) (sliding-window size (rest coll))))))

(defn key?
  [hashes]
  (if-let [[_ c] (re-find #"(.)\1\1" (first hashes))]
    (let [needle (apply str (repeat 5 c))]
      (some
       #(clojure.string/includes? % needle)
       (rest hashes)))))

(defn inputs [prefix] (for [i (iterate inc 0)] (str prefix i)))

(defn stretched-md5
  [input]
  (reduce
   (fn [acc _] (advent-of-code.md5/md5 acc))
   (advent-of-code.md5/md5 input)
   (range 0 2016)))

(defn valid-keys
  [hashes]
  (map first (filter
              (fn [[_ x]] (key? x))
              (map-indexed
               (fn [i x] [i x])
               (sliding-window 1001 hashes)))))

(defn main
  []
  (let [prefix (clojure.string/trim (slurp *in*))]
    (println (nth (valid-keys (map advent-of-code.md5/md5 (inputs prefix))) 63))
    (println (nth (valid-keys (map stretched-md5 (inputs prefix))) 63))))

(main)
