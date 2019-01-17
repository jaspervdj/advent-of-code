(defn vowel? [c] (contains? (set "aeiou") c))

(defn three-vowels? [s] (>= (count (filter vowel? s)) 3))
(defn double-char? [s] (boolean (re-find #"(\w)\1" s)))
(defn no-bads? [s] (every? #(< (.indexOf s %) 0) '("ab" "cd" "pq" "xy")))

(defn nice1? [s] (and (three-vowels? s) (double-char? s) (no-bads? s)))

(defn repeated-pair? [s] (boolean (re-find #"(\w\w).*\1" s)))
(defn has-xyx? [s] (boolean (re-find #"(\w)\w\1" s)))

(defn nice2? [s] (and (repeated-pair? s) (has-xyx? s)))

(defn main
  []
  (let [inputs (clojure.string/split-lines (slurp *in*))]
    (println (count (filter nice1? inputs)))
    (println (count (filter nice2? inputs)))))

(main)
