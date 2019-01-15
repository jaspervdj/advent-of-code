(defrecord Present [l w h])

(defn parse-present
  [line]
  (when-let [[_ l w h] (re-find #"^(\d+)x(\d+)x(\d+)$" line)]
    (Present. (Integer. l) (Integer. w) (Integer. h))))

(defn volume [{l :l w :w h :h}] (* l w h))

(defn faces [{l :l w :w h :h}] [[l w] [w h] [h l]])

(defn paper
  [p]
  (let [sides (map #(apply * %) (faces p))]
    (+ (reduce + 0 (map #(* 2 %) sides)) (apply min sides))))

(defn ribbon
  [p]
  (let [perimeters (map #(* 2 (apply + %)) (faces p))]
    (+ (apply min perimeters) (volume p))))

(defn main
  []
  (let [presents (->> (slurp *in*)
                      clojure.string/split-lines
                      (map parse-present))]

    (println (reduce + 0 (map paper presents)))
    (println (reduce + 0 (map ribbon presents)))))

(main)
