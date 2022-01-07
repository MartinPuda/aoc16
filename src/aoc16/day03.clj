(ns aoc16.day03
  (:require [clojure.string :as s])
  (:gen-class))

(defn get-input []
  (->> (-> "resources/public/input03.txt"
           slurp
           s/trim
           (s/split #"\s+"))
       (map parse-long)
       (partition 3 3)))

(defn valid-triangle? [[x y z]]
  (and (> (+ x y) z)
       (> (+ x z) y)
       (> (+ y z) x)))

(defn part1 [input]
  (->> input
       (filter valid-triangle?)
       count))

(defn part2 [input]
  (->> input
       (apply map vector)
       (apply concat)
       (partition 3 3)
       part1))
