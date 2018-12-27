(defn lcd-create
  [w h]
  (make-array Boolean/TYPE h w))

(defn lcd-print
  [lcd]
  (doseq [l lcd] (println (apply str (map #(if % \# \.) l)))))

(defn lcd-rect
  [lcd w h]
  (doseq [y (range 0 h)
          x (range 0 w)] (aset lcd y x true)))

(defn lcd-rotate-column
  [lcd x by]
  (let [h (alength lcd)
        copy (vec (for [y (range 0 h)] (aget lcd y x)))]
    (doseq [y (range 0 h)]
      (aset lcd y x (nth copy (mod (- y by) h))))))

(defn lcd-rotate-row
  [lcd y by]
  (let [copy (aclone (aget lcd y))
        w (alength copy)]
    (doseq [x (range 0 w)]
      (aset lcd y x (aget copy (mod (- x by) w))))))

(defn lcd-do-instruction
  [lcd instr]
  (let [instrs {:rect lcd-rect
                :rotate-column lcd-rotate-column
                :rotate-row lcd-rotate-row}]
    (apply ((first instr) instrs) lcd (rest instr))))

(defn lcd-count
  [lcd]
  (count (filter (fn [p] p) (for [y (range 0 (alength lcd))
                                  x (range 0 (alength (aget lcd y)))]
                              (aget lcd y x)))))

(defn parse-instruction
  [line]
  (if-let
    [[_ w h] (re-find #"^rect (\d+)x(\d+)$" line)]
    [:rect (Integer. w) (Integer. h)]
    (if-let
      [[_ y by] (re-find #"^rotate row y=(\d+) by (\d+)$" line)]
      [:rotate-row (Integer. y) (Integer. by)]
      (if-let
        [[_ x by] (re-find #"^rotate column x=(\d+) by (\d+)$" line)]
        [:rotate-column (Integer. x) (Integer. by)]
        nil))))

(defn main
  []
  (let [instrs (map
                parse-instruction
                (clojure.string/split-lines (slurp *in*)))
        lcd (lcd-create 50 6)]
    (doseq [instr instrs] (lcd-do-instruction lcd instr))
    (println (lcd-count lcd))
    (lcd-print lcd)))

(main)
