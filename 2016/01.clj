(require 'advent-of-code.list)

(defrecord Pos [x y])

(defn abs [{x :x y :y}] (+ (Math/abs x) (Math/abs y)))

(defn move [pos dir] (case dir
                      :u (update pos :y dec)
                      :r (update pos :x inc)
                      :d (update pos :y inc)
                      :l (update pos :x dec)))

(defn turn-left [dir] (dir {:u :l :r :u :d :r :l :d}))
(defn turn-right [dir] (dir {:u :r :r :d :d :l :l :u}))
(defn turn [dir lr] (if (= lr :l) (turn-left dir) (turn-right dir)))

(defn follow-directions
  "Follow the directions.
  Return a lazy sequence of visited positions."
  [pos dir directions]
  (if-let [[t n] (first directions)]
   (let [dir (turn dir t)
         steps (reductions (fn [a _] (move a dir)) pos (range n))]
     (lazy-seq
      (concat
       (butlast steps)
       (follow-directions (last steps) dir (rest directions)))))
   [pos]))

(defn parse-directions
  "Parse directions, e.g. 'R5, L5, R5, R3'."
  [input]
  (map
   (fn [s] [(keyword (clojure.string/lower-case (subs s 0 1)))
            (Integer. (clojure.string/trim (subs s 1)))])
   (clojure.string/split input #", ")))

(defn main
  []
  (let [directions (parse-directions (slurp *in*))]
    ;; Part 1
    (println (abs (last (follow-directions (Pos. 0 0) :u directions))))
    ;; Part 2
    (println (abs (advent-of-code.list/twice
                   (follow-directions (Pos. 0 0) :u directions))))))

(main)
