(require 'advent-of-code.elf-code)
(use 'advent-of-code.elf-code)

;; We construct a patched program that dumps the value we _want_ r0 to be.
(let [program (parse-program (slurp *in*))
      cmp-instr 28
      cmp-reg (:a (nth (:instructions program) cmp-instr))
      patched-program (assoc program :instructions
        (assoc (:instructions program)
          cmp-instr (->Instruction :seti 999 0 (:ipr program))))
      patched-memory (do-program patched-program (repeat 6 0))

      visited (atom #{})
      hook (fn [mem]
        (let [cmp-val (nth mem cmp-reg)]
          (if (contains? @visited cmp-val)
            (System/exit 0)
            (do
              (println "Visited:" (count @visited) "value" cmp-val)
              (swap! visited conj cmp-val)))))
      hooks (assoc (vec (repeat 29 nil)) cmp-instr hook)]
  (do
    (println (nth patched-memory cmp-reg)))
    (do-program program (repeat 6 0) false hooks))
