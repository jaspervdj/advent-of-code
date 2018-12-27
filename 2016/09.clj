(defn decompress-count
  ([buf] (decompress-count buf false 0 (count buf)))
  ([buf rec] (decompress-count buf rec 0 (count buf)))
  ([buf rec i j]
    (if
     (>= i j)
     0
     (if-not
      (= (nth buf i) \()
      ;; Yield a single character if there's no \(.
      (+ 1 (decompress-count buf rec (+ i 1) j))
      ;; Otherwise, figure out the repeat/len and concat that with the
      ;; rest of the sequence.
      (let [xi (clojure.string/index-of buf \x (+ i 2))
            pi (clojure.string/index-of buf \) (+ xi 2))
            l (Integer. (subs buf (+ i 1) xi))
            r (Integer. (subs buf (+ xi 1) pi))]
        (+
         (* r (if rec (decompress-count buf rec (+ pi 1) (+ pi 1 l)) l))
         (decompress-count buf rec (+ 1 pi l) j)))))))

(defn main
  []
  (let [compressed (clojure.string/trim (slurp *in*))]
    (println (decompress-count compressed))
    (println (decompress-count compressed true))))

(main)
