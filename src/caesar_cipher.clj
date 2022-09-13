(ns caesar-cipher)

(defn decypher [s]
  (for [i (range 1 27)]
    (apply str (char-array (map char
                                (map #(+ (int \A) (mod (+ i %) 26))
                                     (map int (char-array s))))))))

(comment
  (reduce {} #(assoc % (letter-frequency %))
          (decypher "OVDTHUFWVZZPISLRLFZHYLAOLYL.")))

(def average-letter-frequency
  {\A 8.2
   \B 1.5
   \C 2.8
   \D 4.2
   \E 12.7
   \F 2.2
   \G 2.0
   \H 6.1
   \I 7.0
   \J 0.1
   \K 0.8
   \L 4.0
   \M 2.4
   \N 6.7
   \O 7.5
   \P 1.9
   \Q 0.1
   \R 6.0
   \S 6.3
   \T 9.0
   \U 2.8
   \V 1.0
   \W 2.4
   \X 2.0
   \Y 0.1
   \Z 0.1})

(defn letter-frequency [s]
  (let [occurrences (frequencies s)
        percentages (map
                     (fn [m] [(key m) (* 100. (/ (val m)
                                                 (count s)))])
                     occurrences)]
    (into {} percentages)))

(defn substitutes [s]
  (into {}
        (map
         (fn [m]
           [(key m)
            (take 2
             (keys
              (sort #(< (val %1) (val %2))
                    (into {}
                          (map (fn [n]
                                 [(key n)
                                  (abs (- (val m)
                                          (val n)))])
                               average-letter-frequency)))))])
         (letter-frequency s))))

(defn substitute [s a b]
  (apply str (for [c (char-array s)]
               (if (= a c)
                 b
                 c))))
