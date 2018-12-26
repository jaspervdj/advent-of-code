(defrecord Room [encrypted-name sector-id checksum])

(defn ord [c] (- (int c) (int \a)))

(defn shift
  [c n]
  (char (+ (int \a) (mod (+ n (ord c)) (+ (ord \z) 1)))))

(defn decrypted-name
  [room]
  (apply str (map
              (fn [c] (if (= c \-) \- (shift c (:sector-id room))))
              (seq (:encrypted-name room)))))

(defn parse-room
  [line]
  (let [[_ n i c] (re-find #"^([\w-]+)-(\d+)\[(\w+)\]$" line)]
    (Room. n (Integer. i) c)))

(defn parse-rooms
  [input]
  (map parse-room (clojure.string/split-lines input)))

(defn checksum
  [n]
  (let [freqs (frequencies (remove #(= \- %) (seq n)))]
    (apply str (map first (take 5 (reverse (sort-by
                                            (fn [[k v]] [v (-' (int k))])
                                            freqs)))))))

(defn real? [room] (= (:checksum room) (checksum (:encrypted-name room))))

(defn main
  []
  (let [rooms (parse-rooms (slurp *in*))]
    (println (reduce + (map :sector-id (filter real? rooms))))
    (println
     (:sector-id (first (filter
                         #(= "northpole-object-storage" (decrypted-name %))
                         rooms))))))

(main)
