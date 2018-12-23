(import java.lang.Integer)

(defrecord V3 [x y z])

(def origin (V3. 0 0 0))

(defn exp-neighbours
  [v3]
  (for [op [-' +]
        e (range 0 20)
        c [:x :y :z]]
    (update v3 c #(op % (int (Math/pow 2 e))))))

(defn distance
  [l r]
  (+
   (Math/abs (- (:x l) (:x r)))
   (Math/abs (- (:y l) (:y r)))
   (Math/abs (- (:z l) (:z r)))))

(defrecord Nanobot [pos range])

(defn parse-nanobot
  [input]
  (let [[x y z r] (map
                   #(Integer/parseInt %)
                   (remove empty? (clojure.string/split input #"[^\d-]+")))]
    (Nanobot. (V3. x y z) r)))

(defn better
  [[x-pos x-score] [y-pos y-score]]
  (if (< 0 (compare x-score y-score)) [x-pos x-score] [y-pos y-score]))

(defn local-search
  [start scoring neighbours]
  (loop [current-pos start
         current-score (scoring start)]
    (let [[new-pos new-score] (reduce
                               better
                               [current-pos current-score]
                               (map
                                (fn [p] [p (scoring p)])
                                (neighbours current-pos)))]
      (if
       (= new-score current-score)
       [current-pos current-score]
       (recur new-pos new-score)))))

(defn repeated-local-search
  [make-start scoring neighbours]
  (loop [best (local-search (make-start) scoring neighbours)
         searches 0]
    (do
      (println "searches =" searches ", best =" best))
      (recur
       (better best (local-search (make-start) scoring neighbours))
       (+ 1 searches))))

(defn main
  []
  (let [nanobots (map
                  parse-nanobot
                  (clojure.string/split-lines (slurp *in*)))
                  ;; (clojure.string/split-lines (slurp "sample2"))))

        strongest (apply max-key :range nanobots)

        in-range (filter
                  #(<= (distance (:pos %) (:pos strongest)) (:range strongest))
                  nanobots)

        scoring (fn [pos]
                  (let [num-bots (count
                                  (filter
                                   #(<= (distance pos (:pos %)) (:range %))
                                   nanobots))]
                    [num-bots (- (distance pos origin))]))

        random-pos (fn [] (:pos (nth nanobots (rand-int (count nanobots)))))]

    (println (count in-range))
    (repeated-local-search random-pos scoring exp-neighbours)))

(main)
