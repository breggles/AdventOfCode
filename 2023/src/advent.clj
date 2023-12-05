(ns advent
  (:require [clojure.string :as string]
            [clojure.set :as st]
            [clojure.pprint :as pp])
  (:use [data]))

; Day 1

(def names->nums [["one" 1] ["two" 2] ["three" 3] ["four" 4] ["five" 5] ["six" 6] ["seven" 7] ["eight" 8] ["nine" 9]])

(defn repl-strs [line]
  (reduce #(clojure.string/replace %1 (re-pattern (first %2)) (str (second %2)))
          line
          names->nums))

(comment

  (->> (string/split input-day1 #"\n")
       (mapv repl-strs)
       (mapv #(mapv clojure.edn/read-string (re-seq #"\d" %)))
       (mapv (juxt first last))
       (mapv #(+ (* 10 (first %)) (second %)))
       (apply +))
  )
