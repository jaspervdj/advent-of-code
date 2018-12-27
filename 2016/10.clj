(defrecord Bot [low high])

(defn parse-instructions
  [input]
  (let [bot-pat #"^bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)$"
        inp-pat #"^value (\d+) goes to bot (\d+)$"]
    (reduce
     (fn [[bots inputs] line]
       (if-let
        [[_ inp bot] (re-find inp-pat line)]
        [bots (conj inputs [[:bot (Integer. bot)] (Integer. inp)])]
        (if-let
          [[_ bot loty lon hity hin] (re-find bot-pat line)]
          [(assoc bots (Integer. bot) (Bot.
                                       [(keyword loty) (Integer. lon)]
                                       [(keyword hity) (Integer. hin)]))
           inputs]
          (throw (Exception. (str "bad input: " line))))))
     [{} nil]
     (clojure.string/split-lines input))))

(defn step
  [queues bots f]
  (reduce
   (fn [acc [botnum bot]]
     (let [items (get acc [:bot botnum])
           heads (take 2 items)]
       (if
        (>= (count heads) 2)
        (do
          (f botnum heads)
          (assoc
           acc
           (:low bot) (conj (get acc (:low bot)) (apply min heads))
           (:high bot) (conj (get acc (:high bot)) (apply max heads))
           [:bot botnum] (drop 2 items)))
        acc)))
   queues
   bots))

(defn run
  [queues bots f]
  (loop [q queues]
    (if
     (some (fn [[[ty _] l]] (and (= ty :bot) (>= (count l) 2))) q)
     (recur (step q bots f))
     q)))

(defn main
  []
  (let [[bots inputs] (parse-instructions (slurp *in*))
        queues (reduce (fn [acc [k v]] (update acc k conj v)) {} inputs)
        final (run queues bots (fn [botnum heads]
                                 (when (= [17 61] (sort heads))
                                   (println botnum))))]
    (println (reduce * (concat
                        (get final [:output 0])
                        (get final [:output 1])
                        (get final [:output 2]))))))

(main)
