(ns advent
  (:use [data]))

; Day 1

(comment

  (->> (clojure.string/split input-day1 #"\n\n")
       (map #(clojure.string/split % #"\n"))
       (map #(apply + (map (fn [x] (Integer/parseInt x)) %)))
       (apply max))

  (->> (clojure.string/split input #"\n\n")
       (map #(clojure.string/split % #"\n"))
       (map #(apply + (map (fn [x] (Integer/parseInt x)) %)))
       (sort)
       (reverse)
       (take 3)
       (apply +))
)

; Day 2

(defn dist [a b]
  (mod (- a (dec b)) 3))

(defn char->num [c base]
  (- (int c) (int base)))

(defn chars->nums [[[c1] [c2]]]
  [(char->num c1 \A)
    (char->num c2 \X)])

(defn round-points-part-i [[n1 n2]]
  (+ (* 3 (dist n2 n1))
      (inc n2)))

(defn round-points-part-ii [[n1 n2]]
  (+ (* 3 n2)
      (inc (mod (+ n1 (dec n2)) 3))))

(defn points [round-points]
  (->> (clojure.string/split input-day2 #"\n")
        (map #(clojure.string/split % #" "))
        (map chars->nums)
        (map round-points)
        (apply +)))

(comment

  (points round-points-part-i)

  (points round-points-part-ii)

)

; Day 3

(defn priority [c]
  (let [ascii (int c)]
    (if (> ascii (int \Z))
      (inc  (- ascii (int \a)))
      (+ 27 (- ascii (int \A))))))

(->> (clojure.string/split input-day3 #"\n")
     (map #(split-at (/ (count %) 2) %))
     (map #(map set %))
     (map #(apply clojure.set/intersection %))
     (map first)
     (map priority)
     (apply +))

(->> (clojure.string/split input-day3 #"\n")
     (map set)
     (partition 3)
     (map (partial apply clojure.set/intersection))
     (map first)
     (map priority)
     (apply +))

; Day 4
