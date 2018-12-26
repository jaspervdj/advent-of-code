(defn main
  []
  (let [repetitions (clojure.string/split-lines (slurp *in*))
        len (count (first repetitions))
        freqs (for [i (range 0 len)] (frequencies (map #(nth % i) repetitions)))
        message (fn [sel] (apply str (map #(key (apply sel val %)) freqs)))]
    (println (message max-key))
    (println (message min-key))))

(main)
