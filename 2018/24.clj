(require 'advent-of-code.list)
(use 'advent-of-code.list)

(defrecord Group
  [units hit-points attack-damage attack-type initiative weak immune])

(defn effective-power [group] (* (:units group) (:attack-damage group)))

(defn damage
  [attack defend]
  (let [base-damage (effective-power attack)]
    (cond
      (in? (:immune defend) (:attack-type attack)) 0
      (in? (:weak defend) (:attack-type attack)) (* 2 base-damage)
      :else base-damage)))

(defn parse-group
  "Parse a single group from a line"
  [line]
  (let [units (Integer. (second (re-find #"(\d+) units" line)))
        hit-points (Integer. (second (re-find #"(\d+) hit points" line)))
        attack (re-find #"(\d+) ([^\s]+) damage" line)
        attack-damage (Integer. (nth attack 1))
        attack-type (nth attack 2)
        initiative (Integer. (second (re-find #"initiative (\d+)" line)))
        weak-match (re-find #"weak to ([^;)]+)" line)
        immune-match (re-find #"immune to ([^;)]+)" line)
        weak (when-not (nil? weak-match)
                (clojure.string/split (second weak-match) #", "))
        immune (when-not (nil? immune-match)
                  (clojure.string/split (second immune-match) #", "))]
    (Group. units hit-points attack-damage attack-type initiative weak immune)))

(defn parse-team
  "Parse a team of groups"
  [lines]
  (let [header (first lines)
        tname (subs header 0 (- (count header) 1))  ;; Drop the ":".
        groups (map parse-group (rest lines))]
    (for [[g i] (map vector groups (range 0 (count groups)))]
      [[tname (+ i 1)] g])))  ;; +1 indices so they match the examples.

(defn parse-teams
  [input]
  (let [lines (clojure.string/split-lines input)
        [xs ys] (split-with #(not (empty? %)) lines)]
    (into {} (concat (parse-team xs) (parse-team (rest ys))))))

(defn attackers
  "Groups that have at least one unit"
  [groups]
  (for [[k g] groups :when (> (:hit-points g) 0)] [k g]))

(defn target-selection
  "Have all groups select their targets."
  [groups]
  (let [;; Determine in which order the groups will select targets.
        priority (fn [[k g]] [(effective-power g) (:initiative g)])
        queue (reverse (sort-by priority (attackers groups)))]

  (into {}
   (:selected
    (reduce
     (fn [acc [i attack]]
       (let [;; i is the index of the attacker and j is the index of the
             ;; defender.
             targets (for [[j defend] (:available acc)
                           :let [dmg (damage attack defend)
                                 pow (effective-power defend)
                                 ini (:initiative defend)]
                           :when (not (= (first i) (first j)))  ;; Other team
                           :when (> dmg 0)                      ;; Damage
                           :when (> (:units defend) 0)]         ;; Target alive
                       [j [dmg pow ini]])

             j (first (max-by second targets))]

         ;; hopefully we selected a target, otherwise we continue with acc.
         (if
          (nil? j)
          acc
          {:available (dissoc (:available acc) j)
           :selected (conj (:selected acc) [i j])})))
     ;; Targets that have not been selected already, and the selection.
     {:available groups :selected nil}
     queue)))))

(defn attacking
  [groups targets]
  (let [;; Determine attacking order.
        queue (reverse
               (sort-by (fn [[k g]] (:initiative g)) (attackers groups)))]

    ;; Loop through queue and perform attacks.
    (reduce
     (fn [acc [i _]]
       (let [attack (get acc i)
             j (get targets i)]
         (if
          ;; Attacker must still be alive, and it must have a target (j).
          (and (> (:units attack) 0) (not (nil? j)))
          (let [defend (get acc j)
                dmg (damage attack defend)
                killed (min (:units defend) (quot dmg (:hit-points defend)))]
            (assoc acc j (update defend :units #(- % killed))))
          acc)))
     groups
     queue)))

(defn count-units
  "Count the number of alive units on each side."
  [groups]
  (apply merge-with + (for [[[t _] g] groups] {t (:units g)})))

(defn winner
  "Select the winner and the number of units still alive"
  [groups]
  (let [winners (filter (fn [[k u]] (< 0 u)) (count-units groups))]
    (when (= 1 (count winners)) (first winners))))

(defn battle
  "Perform the battle"
  [groups]
  (loop [before groups]
    (let [units-before (count-units before)
          targets (target-selection before)
          after (attacking before targets)
          units-after (count-units after)]
      (cond
        (some zero? (vals units-after)) after  ;; Battle ended
        (= units-before units-after) after     ;; Stalemate
        :else (recur after)))))                ;; Battle continues

(defn boost
  "Boost a specific team"
  [groups tname b]
  (into {} (for [[[tn i] g] groups]
             [[tn i]
              (if (= tn tname) (update g :attack-damage #(+ b %)) g)])))

(defn main
  []
  (let [teams (parse-teams (slurp *in*))]
    ;; Part 1
    (println (second (winner (battle teams))))
    ;; Part 2
    (println
     (loop [b 0]
       (let [r (battle (boost teams "Immune System" b))
             w (winner r)]
         (if (= "Immune System" (first w)) (second w) (recur (+ 1 b))))))))

(main)
