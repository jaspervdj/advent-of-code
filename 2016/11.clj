(require 'clojure.set)
(require 'advent-of-code.dijkstra)

(defn parse-floors
  "Parse the floors."
  [input]
  (vec
   (for [line (clojure.string/split-lines input)
         :let [gens (map second (re-seq #"(\w+) generator" line))
               chips (map second (re-seq #"(\w+)-compatible microchip" line))]]
     (set (concat
           (map (fn [g] [:generator g]) gens)
           (map (fn [c] [:microchip c]) chips))))))

(defn canonicalize
  "This is really the key optimization."
  [floors]
  (let [[elems _] (reduce
                   (fn [[acc i] [_ el]]
                     (if (contains? acc el) [acc i] [(assoc acc el i) (+ i 1)]))
                   [{} 0]
                   (mapcat concat floors))]
    (vec (map
          (fn [floor] (set (map
                            (fn [[ty el]] [ty (get elems el)])
                            floor)))
          floors))))

(defn pick
  "Pick 1 element from a set, returning that element and the resulting set."
  [coll] (for [x coll] [x (disj coll x)]))

(defn pick-1-or-2
  "Pick either one or two elements from a set."
  [coll]
  (apply concat (for [[x xcoll] (pick coll)]
                  (conj
                   (for [[y ycoll] (pick xcoll)
                          :when (< (compare x y) 0)]
                     [#{x y} ycoll])
                   [#{x} xcoll]))))

(defn explodes
  "Determine if a set of items is safe"
  [floor]
  (some
   (fn [[ty el]]
     (case ty
      :generator false
      :microchip (and
                  (not (contains? floor [:generator el]))
                  (some (fn [[t _]] (= :generator t)) floor))))
   floor))

(defn neighbours
  [[floors from]]
  (for [to [(- from 1) (+ from 1)]
        [bring leave] (pick-1-or-2 (nth floors from))
        :when (and (>= to 0) (< to (count floors)))
        :when (not (explodes leave))
        :let [tofloor (clojure.set/union bring (nth floors to))]
        :when (not (explodes tofloor))]
    [1 [(canonicalize (assoc floors to tofloor from leave)) to]]))

(defn goal?
  [[floors elevator]]
  (and
   (= (+ 1 elevator) (count floors))
   (not (some #(not (empty? %)) (butlast floors)))))

(defn steps
  [floors]
  (let [dists (advent-of-code.dijkstra/dijkstra
               [(canonicalize floors) 0]
               neighbours
               goal?)]
    (first (for [[s d] dists :when (goal? s)] d))))

(defn main
  []
  (let [floors (parse-floors (slurp *in*))
        extra #{[:generator "elerium"] [:microchip "elerium"]
                [:generator "dilithium"] [:microchip "dilithium"]}
        actual (update floors 0 clojure.set/union extra)]
    (println (steps floors))
    (println (steps actual))))

(main)
