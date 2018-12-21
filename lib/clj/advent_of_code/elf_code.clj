(ns advent-of-code.elf-code)

(defrecord Instruction [op a b c])

(defn parse-instruction
  [line]
  (let [words (clojure.string/split line #" ")
        op (keyword (nth words 0))
        a (Integer. (nth words 1))
        b (Integer. (nth words 2))
        c (Integer. (nth words 3))]
    (Instruction. op a b c)))

(defrecord Program [ipr instructions])

(defn parse-program
  [input]
  (let [lines (clojure.string/split-lines input)
        ip-line (nth lines 0)
        ipr (Integer. (nth (clojure.string/split ip-line #" ") 1))
        instructions (vec (map parse-instruction (drop 1 lines)))]
    (Program. ipr instructions)))

(defn bool [b] (if b 1 0))

(def operations
  {:addr (fn [mem a b c] (assoc! mem c (+ (nth mem a) (nth mem b))))
   :addi (fn [mem a b c] (assoc! mem c (+ (nth mem a) b)))
   :mulr (fn [mem a b c] (assoc! mem c (* (nth mem a) (nth mem b))))
   :muli (fn [mem a b c] (assoc! mem c (* (nth mem a) b)))
   :banr (fn [mem a b c] (assoc! mem c (bit-and (nth mem a) (nth mem b))))
   :bani (fn [mem a b c] (assoc! mem c (bit-and (nth mem a) b)))
   :borr (fn [mem a b c] (assoc! mem c (bit-or (nth mem a) (nth mem b))))
   :bori (fn [mem a b c] (assoc! mem c (bit-or (nth mem a) b)))
   :setr (fn [mem a b c] (assoc! mem c (nth mem a)))
   :seti (fn [mem a b c] (assoc! mem c a))
   :gtir (fn [mem a b c] (assoc! mem c (bool (> a (nth mem b)))))
   :gtri (fn [mem a b c] (assoc! mem c (bool (> (nth mem a) b))))
   :gtrr (fn [mem a b c] (assoc! mem c (bool (> (nth mem a) (nth mem b)))))
   :eqir (fn [mem a b c] (assoc! mem c (bool (== a (nth mem b)))))
   :eqri (fn [mem a b c] (assoc! mem c (bool (== (nth mem a) b))))
   :eqrr (fn [mem a b c] (assoc! mem c (bool (== (nth mem a) (nth mem b)))))

   ;; Custom operations for part 2.
   :x-is-divisor
     (fn [mem a b c] (assoc! mem c (bool (== 0 (mod (nth mem a) (nth mem b))))))
   :x-pass (fn [mem a b c] mem)})

(defn do-instruction
  [mem instr]
  ((operations (:op instr)) mem (:a instr) (:b instr) (:c instr)))

(defn do-program
  ([program initial-memory]
    (do-program program initial-memory false []))
  ([program initial-memory debug hooks]
    (loop [mem (transient (vec initial-memory))
           ip 0]
      (if (or (< ip 0) (>= ip (count (:instructions program))))
        ;; Stop if the instruction pointer goes outside of the valid range.
        (persistent! mem)
        ;; Otherwise, first write the IP to the right register
        (let [before (assoc! mem (:ipr program) ip)
              ;; Execute the instruction
              instr (nth (:instructions program) ip)
              after (do-instruction before instr)
              ;; Go to the next instruction
              next-instruction (+ 1 (nth after (:ipr program)))]
           (do
             (when debug
               (println "ip =" ip "--" instr)
               (println "after ="
                        (map (fn [i] (nth mem i)) (range 0 (count mem)))))
             (when (and (< ip (count hooks)) (not (nil? (nth hooks ip))))
               ((nth hooks ip) mem))
             (recur after next-instruction)))))))
