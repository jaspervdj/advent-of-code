(use 'advent-of-code.assembunny)

(defn main
  []
  (let [instructions (parse-instructions (slurp *in*))]
    (println (:a (run instructions)))
    (println (:a (run instructions {:c 1})))))

(main)