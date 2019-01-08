(use 'advent-of-code.assembunny)

(defn main
  []
  (let [instructions (parse-instructions (slurp *in*))]
    (println (:a (run instructions {:a 7})))
    (println (:a (run instructions {:a 12})))))

(main)
