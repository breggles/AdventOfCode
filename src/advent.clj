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

; Day 7

(defonce input-day7-test "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn change-dir [acc dir-name]
  (assoc acc :curr-dir
         (if (= ".." dir-name)
           (pop (acc :curr-dir))
           (conj (acc :curr-dir) dir-name))))

(comment
  (-> {:curr-dir [] :fs {"/" {:size 0}}}
      (change-dir "/")
      (change-dir "a")
      (change-dir "..")
      ))

(defn new-dir [acc dir-name]
  (update-in acc
             (cons :fs (acc :curr-dir))
             #(assoc % dir-name {:size 0})))

(comment
  (-> {:curr-dir ["/"] :fs {"/" {:size 0}}}
      (new-dir "a")
      (new-dir "b")
      (change-dir "a")
      (new-dir "c")
      ))

(defn new-file [acc file-size]
  (update-in acc
             (conj (vec (cons :fs (acc :curr-dir))) :size)
             + (Integer/parseInt file-size)))

(comment
  (-> {:curr-dir ["/"] :fs {"/" {:size 0}}}
      (new-file "123")
      (new-file "123")
      (new-dir "b")
      (change-dir "b")
      (new-file "1337")
      ))

(defn parse-fs [acc line]
  (let [[part1 part2] (string/split line #" ")]
    (condp = part1
      "cd"  (change-dir acc part2)
      "ls"  acc
      "dir" (new-dir acc part2)
      (new-file acc part1))))

(defonce fs
  (-> (string/replace input-day7 "$ " "")
      (string/split #"\n")
      (#(reduce parse-fs {:curr-dir [] :fs {"/" {:size 0}}} %))
      (:fs)
      ))

(defn comp-seq-num [x y]
  (cond
    (and (seq?    x) (number? y))  1
    (and (number? x) (seq?    y)) -1
    :else                          0))

(defn sizes [node]
  (if (:size node)
    (sort comp-seq-num (vals node))
    node))

(defn sum-dirs [node]
  (if (seq? node)
    (apply list (apply + (first node)
                         (map first (rest node)))
                (rest node))
    node))

(defonce dir-sizes
  (->> (fs "/")
       (clojure.walk/postwalk sizes)
       (clojure.walk/postwalk sum-dirs)
       (flatten)))

(defonce space-required (- 30000000 (- 70000000 (first dir-sizes))))

(comment

  (->> dir-sizes
       (filter (partial >= 100000))
       (apply +)
       )

  (->> dir-sizes
       (filter (partial <= space-required))
       (apply min)
       ))

; Day 8 - part 1

(defonce input-day8-test
"30373
25512
65332
33549
35390")

(defn rotate [coll]
  (apply mapv vector (reverse coll)))

(defn init-visibility-field [size]
  (->> false (repeat size) (vec)
             (repeat size) (vec)))

(defn parse-forrest [forrest-string]
  (->> (string/split forrest-string #"\n")
       (map (partial map str))
       (map (partial map #(Integer/parseInt %)))
       (mapv vec)))

(defn update-visibility [curr-visibility row max-row]
  (->> (map - row max-row)
       (map pos?)
       (map #(or %1 %2) curr-visibility)))

(defn row-visibility [acc row-num row]
  (-> acc
      (update-in [:visibility row-num]
                 update-visibility
                 row (acc :max-row))
      (update :max-row (partial map max row))))

(defn directional-visibility [visibility forrest]
  (-> (reduce-kv row-visibility
                 {:visibility visibility
                  :max-row    (repeat (count forrest) -1)}
                 forrest)
      (:visibility)
      (rotate)))

(defn visibility-field [forrest]
  (reduce directional-visibility
          (init-visibility-field (count forrest)) ; assume forrest is square
          (take 4 (iterate rotate forrest))))

(comment
  (->> (parse-forrest input-day8)
       (visibility-field)
       (flatten)
       (filter identity)
       (count))
)

; Day 8 - part 2

(defn parse-forrest-2 [forrest-string]
  (->> (string/replace forrest-string #"\n" "")
       (map str)
       (mapv #(Integer/parseInt %))))

(defn forrest-width [forrest]
  (Math/sqrt (count forrest)))

(defn direction [n pred step-fn]
  (take-while pred
              (rest (iterate step-fn n))))

(defn up [n forrest]
  (direction n
            (partial < -1)
            #(- %  (forrest-width forrest))))

(defn down [n forrest]
  (direction n
             (partial > (count forrest))
             (partial + (forrest-width forrest))))

(defn left [n forrest]
  (let [row-start (- n (mod n (forrest-width forrest)))]
    (direction n
               (partial <= row-start)
               dec)))

(defn right [n forrest]
  (let [forrest-width (forrest-width forrest)
        row-end       (+ forrest-width (- n (mod n forrest-width)))]
    (direction n
               (partial > row-end)
               inc)))

(defn direction-view [forrest tree-num view-direction]
  (let [tree-height (nth forrest tree-num)
        [lowers blocked]
          (split-with (partial > tree-height)
                      (map (partial nth forrest)
                           (view-direction tree-num forrest)))]
    (concat lowers (take 1 blocked))))

(defonce directions [up right down left])

(defn scenic-score [forrest n]
  (->> directions
       (map (partial direction-view forrest n))
       (map count)
       (apply *)))

(defn max-scenic-score [forrest]
  (->> (range (count forrest))
       (map (partial scenic-score forrest))
       (apply max)))

(comment
  (max-scenic-score (parse-forrest-2 input-day8))
)

; Day 9

(defonce letter->vec {\U [ 0  1]
                      \R [ 1  0]
                      \D [ 0 -1]
                      \L [-1  0]})

(defn vec* [v scalar]
  (mapv (partial * scalar) v))

(defn vec+ [v1 v2]
  (mapv (partial +) v1 v2))

(defn vec- [v1 v2]
  (vec+ v1 (vec* v2 -1)))

(defn vec->dist [v]
  (apply max (map abs v)))

(defn ht-dist [v1 v2]
  (vec->dist (vec- v1 v2)))

(defn h-path [[curr-pos :as path] [dir cnt]]
  (if (zero? cnt)
    path
    (recur (conj path (vec+ curr-pos dir))
           [dir (dec cnt)])))

(defn sign [n] (/ n (abs n)))

(defn trim-vec [v]
  (mapv #(if (< 1 (abs %)) (* (sign %) (dec (abs %))) %) v))

(defn t-path [[curr-t-pos :as t-path] curr-h-pos]
  (let [[x-dist y-dist :as dist-vec] (vec- curr-h-pos curr-t-pos)]
    (conj t-path (if (< 1 (vec->dist dist-vec))
                   (vec+ (trim-vec dist-vec) curr-t-pos)
                   curr-t-pos))))

(comment

  (->> (re-seq #"([ULDR]) (\d+)" input-day9)
       (map (fn [[_ [dir] cnt]] [(letter->vec dir)
                                 (Integer/parseInt cnt)]))
       (reduce h-path '([0 0]))
       (iterate #(reduce t-path '([0 0]) (reverse %)))
       (drop 9)
       (first)
       (distinct)
       (count)
      )

)

; Day 10

(defn beam-on-sprite? [beam-pos sprite-pos]
  (<= (dec sprite-pos) beam-pos (inc sprite-pos)))

(defn build-screen [screen beam-pos sprite-pos]
  (conj screen
        (if (beam-on-sprite? (mod beam-pos 40) sprite-pos)
          \#
          \.)))

(defonce cycles
  (->> (string/split input-day10 #"\n")
      (map #(string/split % #" "))
      (map #(if (= 2 (count %))
              {:op :addx :val (Integer/parseInt (second %))}
              {:op :noop}))
      (reduce #(if (= :addx (:op %2))
                  (-> %1 (conj (last %1)) (conj (+ (last %1) (:val %2))))
                  (conj %1 (last %1)))
              [1 1])))

(comment

  ; part 1

  (->> (range 20 260 40)
       (map #(* (nth cycles %) %))
       (apply +))

  ; part 2

  (->> (vec (take 240 (rest cycles)))
       (reduce-kv build-screen [])
       (partition 40)
       (map (partial apply str)))
  )

; Day 11

(defmacro monkey [m])
