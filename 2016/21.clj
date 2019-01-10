(require 'advent-of-code.list)

(defn swap-pos
  [x y]
  (let [l (min x y) r (max x y)] (fn [s] (str
                                          (subs s 0 l)
                                          (subs s r (+ r 1))
                                          (subs s (+ l 1) r)
                                          (subs s l (+ l 1))
                                          (subs s (+ r 1))))))

(defn swap-letter
  [a b]
  (let [pat (re-pattern (str \[ a b \]))]
    (fn [s] (clojure.string/replace
             s
             pat
             (fn [x] (if (= (first x) a) (str b) (str a)))))))

(defn rot-steps
  [dir x]
  (let [steps (if (= :left dir) (- x) x)]
    (fn [s] (let [len (count s)
                  idx (mod steps len)]
              (str
               (subs s (- len idx) len)
               (subs s 0 (- len idx)))))))

(defn rot-letter
  [c]
  (fn [s] (let [idx (.indexOf s (str c))]
            ((rot-steps :right (+ 1 idx (if (>= idx 4) 1 0))) s))))

(defn rev
  [x y]
  (let [l (min x y)
        r (max x y)]
    (fn [s] (str
             (subs s 0 l)
             (clojure.string/reverse (subs s l (+ r 1)))
             (subs s (+ r 1))))))

(defn mov
  [x y]
  (fn [s0] (let [s1 (str (subs s0 0 x) (subs s0 (+ x 1)))
                 s2 (str (subs s1 0 y) (subs s0 x (+ x 1)) (subs s1 y))]
             s2)))

(defn parse-operation
  [line]
  (let [p-swap-pos    #"^swap position (\d+) with position (\d+)$"
        p-swap-letter #"^swap letter (\w) with letter (\w)$"
        p-rot-steps   #"^rotate (\w+) (\d+) steps?$"
        p-rot-letter  #"^rotate based on position of letter (\w)$"
        p-rev         #"^reverse positions (\d+) through (\d+)$"
        p-mov         #"^move position (\d+) to position (\d+)$"]
    (if-let [[_ x y] (re-find p-swap-pos line)]
      (swap-pos (Integer. x) (Integer. y))
      (if-let [[_ x y] (re-find p-swap-letter line)]
        (swap-letter (first x) (first y))
        (if-let [[_ x y] (re-find p-rot-steps line)]
          (rot-steps (keyword x) (Integer. y))
          (if-let [[_ x] (re-find p-rot-letter line)]
            (rot-letter (first x))
            (if-let [[_ x y] (re-find p-rev line)]
              (rev (Integer. x) (Integer. y))
              (if-let [[_ x y] (re-find p-mov line)]
                (mov (Integer. x) (Integer. y))
                nil))))))))

(defn parse-operations
  [input]
  (map parse-operation (clojure.string/split-lines input)))

(defn main
  []
  (let [operations (parse-operations (slurp *in*))
        do-operations (fn [in] (reduce (fn [s f] (f s)) in operations))]
    (println (do-operations "abcdefgh"))
    (println (first
               (for [in (advent-of-code.list/permutations (seq "fbgdceah"))
                     :let [out (do-operations (apply str in))]
                     :when (= out "fbgdceah")]
                 (apply str in))))))

(main)
