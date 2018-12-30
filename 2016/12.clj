(defn parse-lit [s] (Integer/parseInt s))
(defn parse-reg [s] (keyword s))
(defn parse-val [s] (try [:lit (Integer/parseInt s)]
                      (catch Exception e [:ref (parse-reg s)])))
(defn parse-instr [line]
  (let [words (clojure.string/split line #"\s+")]
    (case (first words)
      "cpy" [:cpy (parse-val (nth words 1)) (parse-reg (nth words 2))]
      "inc" [:inc (parse-reg (nth words 1))]
      "dec" [:dec (parse-reg (nth words 1))]
      "jnz" [:jnz (parse-val (nth words 1)) (parse-lit (nth words 2))])))

(defn parse-instructions
  [input]
  (vec (map parse-instr (clojure.string/split-lines input))))

(defn run
  ([instructions] (run instructions {}))
  ([instructions initial-memory]
    (let [get-reg (fn [mem r] (or (get mem r) 0))
          get-val (fn [mem [tag x]] (case tag
                                      :lit x
                                      :ref (get-reg mem x)))]
      (loop [mem initial-memory
             ip 0]
        (if
         (or (< ip 0) (>= ip (count instructions)))
         mem
         (let [instr (nth instructions ip)]
           (case (first instr)
             :cpy (recur
                   (assoc mem (nth instr 2) (get-val mem (nth instr 1)))
                   (+ ip 1))
             :inc (recur
                   (assoc mem (nth instr 1) (inc (get-reg mem (nth instr 1))))
                   (+ ip 1))
             :dec (recur
                   (assoc mem (nth instr 1) (dec (get-reg mem (nth instr 1))))
                   (+ ip 1))
             :jnz (if-not
                   (= 0 (get-val mem (nth instr 1)))
                   (recur mem (+ ip (nth instr 2)))
                   (recur mem (+ ip 1))))))))))

(defn main
  []
  (let [instructions (parse-instructions (slurp *in*))]
    (println (:a (run instructions)))
    (println (:a (run instructions {:c 1})))))

(main)
