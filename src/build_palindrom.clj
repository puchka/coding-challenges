(ns build-palindrom)

(defn palindrom? [s]
  (loop [i 0
         b false]
    (if (<= (count s) 1)
      true
      (if (= i (int (/ (count s) 2)))
        b
        (if (= (aget (char-array s) i)
               (aget (char-array s) (- (- (count s) i) 1)))
          (recur (inc i) true)
          false)))))

(defn length-longest-palindrom [s]
  (loop [i (count s)
         c 0]
    (if (= 0 i)
      c
      (if (palindrom? (subs s i))
        (if (> (count (subs s i)) c)
          (recur (dec i) (count (subs s i)))
          (recur (dec i) c))
        (recur (dec i) c)))))

(defn build-palindrom [s]
  (str
   s
   (apply str (reverse (take (- (count s)
                                (length-longest-palindrom s))
                             s)))))
