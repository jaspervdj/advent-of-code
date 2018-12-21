(require 'advent-of-code.elf-code)
(use 'advent-of-code.elf-code)

(let [program (parse-program (slurp *in*))

      ;; These are input-specific instructions to make the program a little
      ;; faster.
      patched-program (assoc program :instructions
        (assoc (:instructions program)
          3 (->Instruction :x-is-divisor 1 4 3)
          4 (->Instruction :addr 3 5 5)
          5 (->Instruction :addi 5 1 5)
          6 (->Instruction :addr 4 0 0)
          7 (->Instruction :seti 11 0 5)))]

  (do
    (println (nth (do-program program (repeat 6 0)) 0))
    (println (nth (do-program patched-program (conj (repeat 5 0) 1)) 0))))
