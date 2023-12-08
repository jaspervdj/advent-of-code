(require 'advent-of-code.grid)

(defn parse-directions [input] (for [x input] (case x \^ :u \> :r \v :d \< :l)))

(defn deliver-presents
  [directions]
  (first (reduce
          (fn [[grid pos] dir]
            (let [npos (advent-of-code.grid/move pos dir)]
              [(update grid npos #(+ 1 (or % 0))) npos]))
          [{advent-of-code.grid/zero 1} advent-of-code.grid/zero]
          directions)))

(defn every-second [items] (apply concat (partition 1 2 items)))

(defn main
  []
  (let [directions (parse-directions (slurp *in*))
        real-santa-directions (every-second directions)
        robo-santa-directions (every-second (rest directions))]
    (println (count (deliver-presents directions)))
    (println (count (merge-with
                     +
                     (deliver-presents real-santa-directions)
                     (deliver-presents robo-santa-directions))))))

(main)
