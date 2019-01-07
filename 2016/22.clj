(require 'advent-of-code.grid)
(require 'advent-of-code.dijkstra)
(use '[advent-of-code.grid :only (->Pos)])

(defrecord Node [pos size used])

(defn avail [node] (- (:size node) (:used node)))

(defn parse-nodes
  [input]
  (let [pat #"^/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T"]
    (keep
     (fn [line]
       (when-let [[_ x y size used] (re-find pat line)]
         (Node.
          (->Pos (Integer. x) (Integer. y))
          (Integer. size)
          (Integer. used))))
     (clojure.string/split-lines input))))

(defn viable-pairs
  [nodes]
  (for [a nodes
        b nodes
        :when (not= (:used a) 0)
        :when (not= (:pos a) (:pos b))
        :when (<= (:used a) (avail b))]
    [a b]))

(defn part2
  [nodes]
  ;; We make some very specific assumptions about the layout of the data.  I
  ;; don't think these are input-specific, though, as the same assumptions
  ;; appear in the given example.

  (let [;; Global properties
        max-x (apply max (map #(:x (:pos %)) nodes))
        empty-start (:pos (first (filter #(= (:used %) 0) nodes)))
        top-right (->Pos max-x 0)

        ;; Big, non-moving blocks are basically walls
        walls (set (for [n nodes
                         :when (> (:size n) 100)]
                     (:pos n)))

        ;; Figure out how long it takes to get the empty spot to the top right.
        dists (advent-of-code.dijkstra/dijkstra
               empty-start
               (fn [pos]
                 (for [nb (advent-of-code.grid/neighbours pos)
                       :when (and (>= (:x nb) 0) (<= (:x nb) max-x))
                       :when (not (contains? walls nb))]
                   [1 nb]))
               #(= top-right %))
        to-top-right (get dists top-right)

        ;; Once the empty spot is there, the data we want is immediately left of
        ;; it.  Shuffling the empty space around, we need 5 steps to move our
        ;; data node 1 to the left.
        to-top-left (* 5 (- max-x 1))]

    ;; Total is easily computed.
    (+ to-top-right to-top-left)))

(defn main
  []
  (let [nodes (parse-nodes (slurp *in*))]
    (println (count (viable-pairs nodes)))
    (println (part2 nodes))))

(main)
