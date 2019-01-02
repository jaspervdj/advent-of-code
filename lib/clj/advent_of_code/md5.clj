(ns advent-of-code.md5)

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5
  [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))
