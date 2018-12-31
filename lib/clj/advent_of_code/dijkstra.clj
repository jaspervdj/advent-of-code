(ns advent-of-code.dijkstra)

(import java.lang.Comparable)
(import java.util.PriorityQueue)

;; Internally used: simple defrecord to implement comparable, so that we can use
;; the priority queue from Java.
(defrecord Visit [distance place]
  Comparable
    (compareTo [this other] (.compareTo (:distance this) (:distance other))))

;; Returns a map of nodes to distances.
(defn dijkstra
  ;; By default, don't use a `goal?`.
  ([start neighbours]
    (dijkstra start neighbours (fn [_] false)))

  ([start neighbours goal?]
    ;; We are using some mutable data structures here.
    (let [queue (java.util.PriorityQueue. [(Visit. 0 start)])]
      ;; Loop with a map of places we have visited already.
      (loop [dists {}]
        (let [visit (.poll queue)]
          (if
            ;; If the queue is empty, return the distances.
            (nil? visit)
            dists
            (if
              ;; If we have already visited this place, continue.
              (contains? dists (:place visit))
              (recur dists)
              ;; If not, we visit this place and its neighbours to the queue.
              (let [ndists (assoc dists (:place visit) (:distance visit))]
                (doseq [[dist nb] (neighbours (:place visit))]
                  (when-not (contains? dists nb)
                    (.add queue (Visit. (+ (:distance visit) dist) nb))))
                ;; If this place was the goal, we are done, otherwise we
                ;; continue.
                (if
                  (goal? (:place visit))
                  ndists
                  (recur ndists))))))))))
