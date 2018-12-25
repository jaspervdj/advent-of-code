(use 'advent-of-code.grid)
(require 'advent-of-code.list)

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
    (println (abs (last (follow-directions (->Pos 0 0) :u directions))))
    ;; Part 2
    (println (abs (advent-of-code.list/twice
                   (follow-directions (->Pos 0 0) :u directions))))))

(main)
