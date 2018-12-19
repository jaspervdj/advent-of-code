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
   :is-divisor
     (fn [mem a b c] (assoc! mem c (bool (== 0 (mod (nth mem a) (nth mem b))))))
   :pass (fn [mem a b c] mem)})

(defn do-instruction
  [mem instr]
  ((operations (:op instr)) mem (:a instr) (:b instr) (:c instr)))

(defn do-program
  [program initial-memory]
  (loop [mem (transient (vec initial-memory))
         ip 0]
    (if (or (< ip 0) (>= ip (count (:instructions program))))
      ;; Stop if the instruction pointer goes outside of the valid range.
      (persistent! mem)
      ;; Otherwise, first write the IP to the right register
      (let [before (assoc! mem (:ipr program) ip)
            ;; Execute the instruction
            after (do-instruction before (nth (:instructions program) ip))]
            ;; Go to the next instruction
         (recur after (+ 1 (nth after (:ipr program))))))))

(let [program (parse-program (slurp *in*))

      ;; These are input-specific instructions to make the program a little
      ;; faster.
      patched-program (assoc program :instructions
        (assoc (:instructions program)
          3 (Instruction. :is-divisor 1 4 3)
          4 (Instruction. :addr 3 5 5)
          5 (Instruction. :addi 5 1 5)
          6 (Instruction. :addr 4 0 0)
          7 (Instruction. :seti 11 0 5)))]

  (do
    (println (nth (do-program program (repeat 6 0)) 0))
    (println (nth (do-program patched-program (conj (repeat 5 0) 1)) 0))))
