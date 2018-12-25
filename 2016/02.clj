(use 'advent-of-code.grid)

(def simple-keypad
  {(->Pos 0 0) 1 (->Pos 1 0) 2 (->Pos 2 0) 3
   (->Pos 0 1) 4 (->Pos 1 1) 5 (->Pos 2 1) 6
   (->Pos 0 2) 7 (->Pos 1 2) 8 (->Pos 2 2) 9})

(def interesting-keypad
  {                             (->Pos 2 0) 1
                 (->Pos 1 1) 2  (->Pos 2 1) 3  (->Pos 3 1) 4
   (->Pos 0 2) 5 (->Pos 1 2) 6  (->Pos 2 2) 7  (->Pos 3 2) 8  (->Pos 4 2) 9
                 (->Pos 1 3) \A (->Pos 2 3) \B (->Pos 3 3) \C
                                (->Pos 2 4) \D })

(defn follow
  [keypad init-pos directions]
  (reduce
   (fn [acc d] (let [pos (move acc d)] (if (contains? keypad pos) pos acc)))
   init-pos
   directions))

(defn bathroom-code
  [keypad init-pos directions]
  (if-let [dirs (first directions)]
    (let [pos (follow keypad init-pos dirs)]
      (lazy-seq
       (conj (bathroom-code keypad pos (rest directions)) (get keypad pos))))
    nil))

(defn parse-directions
  [input]
  (map
   (fn [line] (map #(keyword (str %)) (seq line)))
   (clojure.string/split-lines (clojure.string/lower-case input))))

(defn main
  []
  (let [directions (parse-directions (slurp *in*))
        print-code #(println (clojure.string/join (map str %)))]
     (print-code (bathroom-code simple-keypad (->Pos 1 1) directions))
     (print-code (bathroom-code interesting-keypad (->Pos 0 2) directions))))

(main)
