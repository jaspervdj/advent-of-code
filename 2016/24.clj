(require 'advent-of-code.dijkstra)
(require 'advent-of-code.grid)
(require 'advent-of-code.list)

(defn parse-layout
  [input]
  (let [grid (advent-of-code.grid/parse-grid input)]
    {:grid (reduce-kv #(assoc %1 %2 (not= \# %3)) {} grid)
     :pois (into {} (keep
                     (fn [[p c]] (when (Character/isDigit c)
                                   [(Character/getNumericValue c) p]))
                     grid))}))

(defn layout-neighbours
  [layout pos]
  (for [nb (advent-of-code.grid/neighbours pos)
            :when (= true (get (:grid layout) nb))]
     [1 nb]))

(defn layout-distances
  [layout]
  (into {} (for [[poix posx] (:pois layout)
                 ;; Run dijkstra on this POI.
                 :let [distances (advent-of-code.dijkstra/dijkstra
                                  posx
                                  #(layout-neighbours layout %))]
                 ;; Get distances to all other POIs
                 [poiy posy] (:pois layout)]
             [[poix poiy] (get distances posy)])))

(defn layout-paths
  [layout start]
  (map #(cons start %)
   (advent-of-code.list/permutations
    (filter #(not= start %) (keys (:pois layout))))))

(defn path-cost
  [distances path]
  (reduce
   (fn [acc xy] (+ acc (get distances xy)))
   0
   (map vector path (drop 1 path))))

(defn main
  []
  (let [layout (parse-layout (slurp *in*))
        distances (layout-distances layout)]
    (println (apply min (for [path (layout-paths layout 0)]
                          (path-cost distances path))))
    (println (apply min (for [path (layout-paths layout 0)]
                          (path-cost distances (concat path '(0))))))))

(main)
