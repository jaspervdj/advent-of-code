(ns advent-of-code.assembunny)

(require 'clojure.core.reducers)

(defn parse-val [s] (try (Integer/parseInt s) (catch Exception e (keyword s))))

(defn parse-instr [line]
  (let [words (clojure.string/split line #"\s+")]
    (case (first words)
      "cpy" [:cpy (parse-val (nth words 1)) (parse-val (nth words 2))]
      "inc" [:inc (parse-val (nth words 1))]
      "dec" [:dec (parse-val (nth words 1))]
      "jnz" [:jnz (parse-val (nth words 1)) (parse-val (nth words 2))]
      "tgl" [:tgl (parse-val (nth words 1))]
      "out" [:out (parse-val (nth words 1))])))

(defn parse-instructions
  [input]
  (vec (map parse-instr (clojure.string/split-lines input))))

(defn toggle-instr
  [instr]
  (update instr 0 (fn [op] (if
                             (= 2 (count instr))
                             (if (= :inc op) :dec :inc)
                             (if (= :jnz op) :cpy :jnz)))))

(defn get-val [mem x] (if (keyword? x) (or (get mem x) 0) x))

(defn run-fast
  [instructions mem ip]
  ;; Recognize a multiplication in the form of:
  ;;
  ;; > cpy b c
  ;; > inc a
  ;; > dec c
  ;; > jnz c -2
  ;; > dec d
  ;; > jnz d -5
  (let [[[op0 x0 y0]
         [op1 x1 y1]
         [op2 x2 y2]
         [op3 x3 y3]
         [op4 x4 y4]
         [op5 x5 y5]] (take 6 (drop ip instructions))]
    (if
     (and
      (= op0 :cpy)
      (= op1 :inc)
      (= op2 :dec)
      (= op3 :jnz)
      (= op4 :dec)
      (= op5 :jnz)
      (= y0 x2 x3) ;; :c
      (= x4 x5)    ;; :d
      (= y3 -2)
      (= y5 -5))
     (let [sumul (+ (get-val mem x1) (* (get-val mem x0) (get-val mem x4)))]
       [instructions
        (assoc mem y0 0 x4 0 x1 sumul)
        (+ 6 ip)])
     nil)))

(defn run
  ([initial-instructions] (run initial-instructions {}))
  ([initial-instructions initial-memory]
     (run initial-instructions initial-memory 100))
  ([initial-instructions initial-memory outs-limit]
    (loop [instructions initial-instructions
           mem initial-memory
           outs []
           ip 0]
      (if
       (or (< ip 0) (>= ip (count instructions)) (>= (count outs) outs-limit))
       [mem outs]
       (if-let [[instructions' mem' ip'] (run-fast instructions mem ip)]
         (recur instructions' mem' outs ip')
         (let [[op x y] (nth instructions ip)]
           (case op
             :cpy (if-not
                   (keyword? y) ;; Invalid instruction check.
                   (recur instructions mem outs (+ ip 1))
                   (recur
                    instructions
                    (assoc mem y (get-val mem x))
                    outs
                    (+ ip 1)))
             :inc (recur
                   instructions
                   (assoc mem x (inc (get-val mem x)))
                   outs
                   (+ ip 1))
             :dec (recur
                   instructions
                   (assoc mem x (dec (get-val mem x)))
                   outs
                   (+ ip 1))
             :jnz (if-not
                   (= 0 (get-val mem x))
                   (recur instructions mem outs (+ ip (get-val mem y)))
                   (recur instructions mem outs (+ ip 1)))
             :tgl (let [idx (+ ip (get-val mem x))]
                    (recur
                     (if
                      (and (>= idx 0) (< idx (count instructions)))
                      (update instructions idx toggle-instr)
                      instructions)
                     mem
                     outs
                     (+ ip 1)))
             :out (recur
                   instructions
                   mem
                   (conj outs (get-val mem x))
                   (+ ip 1)))))))))
