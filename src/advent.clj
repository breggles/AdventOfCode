(ns advent
  (:require [clojure.string :as string]
            [clojure.set :as st]
            [clojure.pprint :as pp])
  (:use [data]))

; Day 1

(comment

  (->> (string/split input-day1 #"\n\n")
       (map #(string/split % #"\n"))
       (map #(apply + (map (fn [x] (Integer/parseInt x)) %)))
       (apply max))

  (->> (string/split input #"\n\n")
       (map #(string/split % #"\n"))
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
  (->> (string/split input-day2 #"\n")
        (map #(string/split % #" "))
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

(comment

  (->> (string/split input-day3 #"\n")
       (map #(split-at (/ (count %) 2) %))
       (map #(map set %))
       (map #(apply st/intersection %))
       (map first)
       (map priority)
       (apply +))

  (->> (string/split input-day3 #"\n")
       (map set)
       (partition 3)
       (map (partial apply st/intersection))
       (map first)
       (map priority)
       (apply +))

)

; Day 4

(defn range-contained? [[lb1 hb1 lb2 hb2]]
  (or (<= lb1 lb2 hb2 hb1)
      (<= lb2 lb1 hb1 hb2)))

(defn ranges-overlap? [[lb1 hb1 lb2 hb2]]
  (or (<= lb1 lb2 hb1)
      (<= lb2 hb1 hb2)
      (<= lb2 lb1 hb2)
      (<= lb1 hb2 hb1)))

(defn range-count [range-match?]
  (->> (string/split input-day4 #"\n")
       (map #(string/split % #","))
       (flatten)
       (map #(string/split % #"-"))
       (flatten)
       (map #(Integer/parseInt %))
       (partition 4)
       (filter range-match?)
       (count)))

(comment

  (range-count range-contained?)

  (range-count ranges-overlap?)

)

; Day 5

(defn debug [x] (pp/pprint x) x)

(defonce stacks
  (->> (string/split input-day5-stacks #"\n")
       (map (partial partition-all 4))
       (map (partial map (partial remove #{\space \[ \]})))
       (map (partial map first))
       (apply map vector)
       (mapv (partial remove nil?))))

(defonce moves
  (->> (string/split input-day5-moves #"\n")
       (map #(string/split % #" "))
       (flatten)
       (partition 2)
       (map second)
       (map #(Integer/parseInt %))
       (partition 3)
       (map (partial interleave [:count :from :to]))
       (map (partial apply hash-map))
       (map #(update % :from dec))
       (map #(update % :to dec))))

; (def moves
;   (->> (string/split input-day5-moves #"\n")
;        (take 10)
;        (map (partial re-seq #"\d"))
;        (map (partial map #(Integer/parseInt %)))
;        (map (partial interleave [:count :from :to]))
;        (map (partial apply hash-map))
;        (map #(update % :from dec))
;        (map #(update % :to dec))))

(defn move-crates [stack crates]
  (concat crates stack)
  ; (concat (reverse crates) stack)
  )

(defn update-stacks [stacks move]
  (let [[top bottom] (split-at (:count move)
                               (nth stacks (:from move)))]
    (-> stacks
        (assoc (:from move) bottom)
        (update (:to move) move-crates top))))

(comment

  (->> moves
       (reduce update-stacks stacks)
       (map first)
       (apply str))

)

; Day 6

; (defn find-marker [prev index current-char]
;   (let [marker (conj prev current-char)]
;     (if (apply distinct? marker)
;       (reduced (+ 14 index))
;       (vec (rest marker)))))

; (reduce-kv find-marker
;            (vec (take 13 input-day6))
;            (vec (drop 13 input-day6)))

(->> input-day6
     (partition 4 1)
     (take-while #(not (apply distinct? %)))
     (count)
     (+ 4))
