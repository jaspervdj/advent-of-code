(defn re-seq-overlapping [re s]
  "Like re-seq, but can find overlapping matches."
  (loop [m (re-matcher re s)
         p 0
         res nil]
    (if (.find m p)
     (recur m (+ 1 (.start m)) (conj res (subs s (.start m) (.end m))))
     (reverse res))))

;; Regex "fun"
(defn abba? [ip] (not (nil? (re-find #"(\w)(?!\1)(\w)\2\1" ip))))
(defn hypernets [ip] (re-seq #"\[[^]]*\]" ip))
(defn tls? [ip] (and (abba? ip) (not-any? abba? (hypernets ip))))
(defn supernets [ip] (re-seq #"^\w+|\]\w+\[|\w+$" ip))
(defn abas [ip] (re-seq-overlapping #"(\w)(?!\1)(\w)\1" ip))
(defn bab [aba] (str (nth aba 1) (nth aba 0) (nth aba 1)))
(defn ssl? [ip]
  (some
   (fn [aba] (some #(clojure.string/includes? % (bab aba)) (hypernets ip)))
   (mapcat abas (supernets ip))))

(defn main
  []
  (let [ips (clojure.string/split-lines (slurp "2016/07.txt"))]
    (println (count (filter tls? ips)))
    (println (count (filter ssl? ips)))))

(main)
