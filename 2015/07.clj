(defn parse-lhs
  [lhs]
  (let [[x y z] (clojure.string/split lhs #" +")]
    (cond
      (= nil y z)    (try
                      [:lit (Integer/parseInt x)]
                      (catch Exception e [:var x]))
      (= "NOT" x)    [:not (parse-lhs y)]
      (= "AND" y)    [:and (parse-lhs x) (parse-lhs z)]
      (= "OR" y)     [:or (parse-lhs x) (parse-lhs z)]
      (= "LSHIFT" y) [:lshift (parse-lhs x) (parse-lhs z)]
      (= "RSHIFT" y) [:rshift (parse-lhs x) (parse-lhs z)])))

(defn parse-circuit
  [input]
  (into {} (for [line (clojure.string/split-lines input)
                 :let [[_ lhs rhs] (re-find #"^(.*) -> (.*)$" line)]]
             [rhs (parse-lhs lhs)])))

(defn to16bit [x] (bit-and x 0xffff))

(defn eval-circuit
  ([c instr] (eval-circuit c instr (atom {})))

  ([c [op x y] cache]
   (to16bit
    (case op
      :lit    x
      :var    (if-let [v (get @cache x)]
                v
                (let [v (eval-circuit c (get c x) cache)]
                  (swap! cache assoc x v)
                  v))
      :not    (bit-not (eval-circuit c x cache))
      :and    (bit-and (eval-circuit c x cache) (eval-circuit c y cache))
      :or     (bit-or (eval-circuit c x cache) (eval-circuit c y cache))
      :lshift (bit-shift-left
               (eval-circuit c x cache)
               (eval-circuit c y cache))
      :rshift (bit-shift-right
               (eval-circuit c x cache)
               (eval-circuit c y cache))))))

(defn main
  []
  (let [circuit-1 (parse-circuit (slurp *in*))
        a-out-1 (eval-circuit circuit-1 [:var "a"])
        circuit-2 (assoc circuit-1 "b" [:lit a-out-1])
        a-out-2 (eval-circuit circuit-2 [:var "a"])]
    (println a-out-1)
    (println a-out-2)))

(main)
