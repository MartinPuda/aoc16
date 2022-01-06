(ns aoc16.day02
  (:require [clojure.string :as s])
  (:gen-class))

(defn get-input []
  (-> (slurp "resources/public/input02.txt")))

(defn part1 [input]
  (let [[position history]
        (reduce (fn [[position history] c]
                  (case c
                    \newline [position (conj history position)]
                    \U [(let [new-pos (- position 3)]
                          (if (neg? new-pos) position new-pos)) history]
                    \D [(let [new-pos (+ position 3)]
                          (if (> new-pos 9) position new-pos)) history]
                    \L [(if (#{2 3 5 6 8 9} position) (dec position) position) history]
                    \R [(if (#{1 2 4 5 7 8} position) (inc position) position) history]))
                [5 []]
                input)]
    history))
