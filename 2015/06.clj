(defn grid [w h] (make-array Integer/TYPE h w))

(defn map-area
  [g f x0 y0 x1 y1]
  (doseq [y (range y0 (+ y1 1))
          x (range x0 (+ x1 1))]
    (aset g y x (f (aget g y x)))))

(def operations-v1
  {:turn-on  (fn [_] (int 1))
   :turn-off (fn [_] (int 0))
   :toggle   (fn [x] (if (= x 0) (int 1) (int 0)))})

(def operations-v2
  {:turn-on  (fn [x] (int (inc x)))
   :turn-off (fn [x] (if (> x 0) (int (dec x)) x))
   :toggle   (fn [x] (int (+ x 2)))})

(defn do-instructions
  [operations instructions]
  (reduce
   (fn [g [op x0 y0 x1 y1]] (do (map-area g (op operations) x0 y0 x1 y1) g))
   (grid 1000 1000)
   instructions))

(defn count-lights
  [g]
  (areduce g i acc0 0
    (+ acc0 (areduce (aget g i) j acc1 0 (+ (aget g i j) acc1)))))

(defn parse-instruction
  [line]
  (let [[_ op sx0 sy0 sx1 sy1] (re-find
                                #"^(.+) (\d+),(\d+) through (\d+),(\d+)$"
                                line)
        x0 (Integer. sx0)
        y0 (Integer. sy0)
        x1 (Integer. sx1)
        y1 (Integer. sy1)]
    (case op
      "turn on"  [:turn-on  x0 y0 x1 y1]
      "turn off" [:turn-off x0 y0 x1 y1]
      "toggle"   [:toggle   x0 y0 x1 y1])))

(defn main
  []
  (let [instructions (map
                      parse-instruction
                      (clojure.string/split-lines (slurp *in*)))]
    (println (count-lights (do-instructions
                            operations-v1
                            instructions)))
    (println (count-lights (do-instructions
                            operations-v2
                            instructions)))))

(main)
