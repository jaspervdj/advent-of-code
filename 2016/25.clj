(use 'advent-of-code.assembunny)

(defn clock-signal
  [instructions]
  (let [sample 10
        expected (vec (take sample (cycle [0 1])))]
  (loop [a 0]
    (let [actual (second (run instructions {:a a} sample))]
      (if (= actual expected) a (recur (+ a 1)))))))

(defn main
  []
  (let [instructions (parse-instructions (slurp *in*))]
    (println (clock-signal instructions))))

(main)
