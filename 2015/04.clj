(require 'advent-of-code.md5)

(defn prefixed-md5s
  [prefix input]
  (for [x (iterate inc 1)
        :let [md5 (advent-of-code.md5/md5 (str input x))]
        :when (clojure.string/starts-with? md5 prefix)]
    x))

(defn main
  []
  (let [secret-key (clojure.string/trim (slurp *in*))]
    (println (first (prefixed-md5s "00000" secret-key)))
    (println (first (prefixed-md5s "000000" secret-key)))))

(main)
