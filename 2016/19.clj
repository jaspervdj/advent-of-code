(defrecord Elf [num next])

(defn create-circle
  [num-elfs]
  (let [last-elf (atom nil)
        first-elf (reduce
                   (fn [elfs i] (atom (Elf. i elfs)))
                   last-elf
                   (range (- num-elfs 1) 0 -1))]
    (swap! last-elf (fn [_] (Elf. num-elfs first-elf)))
    first-elf))

(defn skip [elf n] (reduce (fn [e _] (:next @e)) elf (range 0 n)))

(defn kill! [elf] (swap! elf (fn [e] (assoc e :next (:next @(:next e))))))

(defn party1
  [num-elfs]
  (loop [i (create-circle num-elfs)]
    (if (identical? i (:next @i)) (:num @i) (recur (:next (kill! i))))))

(defn party2
  [num-elfs]
  (loop [i (create-circle num-elfs)
         j (skip i (- (quot num-elfs 2) 1))
         n num-elfs]
    (if
     (identical? i (:next @i))
     (:num @i)
     (do
       (kill! j)
       (recur (:next @i) (if (odd? n) (:next @j) j) (- n 1))))))

(defn main
  []
  (let [num-elfs (Integer. (clojure.string/trim (slurp *in*)))]
    (println (party1 num-elfs))
    (println (party2 num-elfs))))

(main)
