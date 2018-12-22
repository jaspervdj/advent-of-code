(require 'advent-of-code.dijkstra)

(defn erosion-levels
  [width height depth target-x target-y]
  (let [idx (fn [x y] (+ (* y width) x))]
    (loop [levels (transient (vec (repeat (* width height) nil)))
           x 0
           y 0]
      (cond
        (>= x width) (recur levels 0 (+ y 1))
        (>= y height) (persistent! levels)
        :else (let [geo-index (cond
                                (and (= 0 x) (= 0 y)) 0
                                (and (= target-x x) (= target-y y)) 0
                                (= 0 y) (* x 16807)
                                (= 0 x) (* y 48271)
                                :else (*
                                        (nth levels (idx (- x 1) y))
                                        (nth levels (idx x (- y 1)))))
                    erosion (mod (+ geo-index depth) 20183)]
                (recur (assoc! levels (idx x y) erosion) (+ x 1) y))))))

(defn risk-level [erosion-lvl] (mod erosion-lvl 3))

(defn region-type [erosion-lvl]
  (case (risk-level erosion-lvl)
    0 :rocky
    1 :wet
    2 :narrow))

(defn can-use
   [equipment ty]
   (case ty
     :rocky (or (= :climbing-gear equipment) (= :torch equipment))
     :wet (or (= :climbing-gear equipment) (= :neither equipment))
     :narrow (or (= :torch equipment) (= :neither equipment))))

;; A state models both the current equipment as well as the position.
(defrecord Node [equipment x y])

;; Transitioning states.
(defn neighbours
  [levels width height node]
  (let [x (:x node)
        y (:y node)
        ty (region-type (nth levels (+ (* y width) x)))]
    ;; Switching equipment
    (concat
      (for [ne [:climbing-gear :torch :neither]
            :when (and (can-use ne ty) (not= (:equipment node) ne))]
        [7 (Node. ne x y)])
      ;; Actually moving
      (for [[nx ny] [[x (- y 1)] [(+ x 1) y] [x (+ y 1)] [(- x 1) y]]
            :when (and
                    (and (>= nx 0) (>= ny 0) (< nx width) (< ny height))
                    (can-use
                      (:equipment node)
                      (region-type (nth levels (+ (* ny width) nx)))))]
        [1 (Node. (:equipment node) nx ny)]))))

(defn main []
  (let [tokens (clojure.string/split (slurp *in*) #"[\s,]+")
        depth (Integer. (nth tokens 1))
        target-x (Integer. (nth tokens 3))
        target-y (Integer. (nth tokens 4))

        levels (erosion-levels
                (+ target-x 1)
                (+ target-y 1)
                depth
                target-x
                target-y)

        bigger-levels (erosion-levels
                       (+ target-x 50)
                       (+ target-y 50)
                       depth
                       target-x
                       target-y)
        target-node (Node. :torch target-x target-y)
        dists (advent-of-code.dijkstra/dijkstra
               (Node. :torch 0 0)
               (fn [n] (neighbours
                         bigger-levels
                         (+ 50 target-x)
                         (+ 50 target-y)
                         n))
               (fn [n] (= n target-node)))]

    (println (reduce + (map risk-level levels)))
    (println (get dists target-node))))

(main)
