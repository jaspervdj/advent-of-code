(use 'advent-of-code.assembunny)

(defn main
  []
  (let [instructions (parse-instructions (slurp *in*))]
    (println (:a (first (run instructions))))
    (println (:a (first (run instructions {:c 1}))))))

(main)
