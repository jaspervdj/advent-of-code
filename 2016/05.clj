(require 'advent-of-code.md5)

(defn interesting-hashes
  [door-id]
  (filter #(clojure.string/starts-with? % "00000")
   (map #(advent-of-code.md5/md5 (str door-id %)) (iterate inc 0))))

(defn part-1-password
  [door-id]
  (map #(nth % 5) (interesting-hashes door-id)))

(defn part-2-password
  [door-id]
  (reduce
   (fn [pw h] (let [pos (Character/getNumericValue (nth h 5))
                    digit (nth h 6)]
                (if
                 (and (>= pos 0) (< pos (count pw)) (= \_ (nth pw pos)))
                 (let [pw (assoc pw pos digit)]
                   (.println *err* (apply str pw))
                   (if (some #(= \_ %) pw) pw (reduced pw)))
                 pw)))
   (vec (repeat 8 \_))
   (interesting-hashes door-id)))

(defn main
  []
  (let [door-id (clojure.string/trim (slurp *in*))]
    (println (apply str (take 8 (part-1-password door-id))))
    (println (apply str (part-2-password door-id)))))

(main)
