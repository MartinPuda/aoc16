(ns aoc16.day01
  (:require [clojure.string :as s]
            [clojure.set :as cljset])
  (:gen-class))

(defn get-input []
  (-> (slurp "resources/public/input01.txt")
      (s/trim-newline)
      (s/split #", ")))

(defn update-state [state rad-angle steps-val]
  (-> state
      (assoc :angle rad-angle)
      (update :x + (Math/round ^double (* steps-val (Math/cos rad-angle))))
      (update :y + (Math/round ^double (* steps-val (Math/sin rad-angle))))))

(defn part1 [input]
  (let [{:keys [x y angle]}
        (reduce (fn [state [side & steps]]
                  (let [steps-val (parse-long (s/join steps))
                        rad-angle (+ (:angle state)
                                     (if (= side \L)
                                       (/ Math/PI 2)
                                       (/ Math/PI -2)))]
                    (update-state state rad-angle steps-val)))
                {:x 0 :y 0 :angle (/ Math/PI 2)}
                input)]
    (+ (Math/abs ^double x) (Math/abs ^double y))))

;TODO: refactor part2

(defn state-difference [state1 state2]
  (let [delta-x (- (:x state2) (:x state1))
        delta-y (- (:y state2) (:y state1))]
    (if (zero? delta-x)
      (->> (range (:y state1) (:y state2) (Math/signum ^double delta-y))
           (map #(hash-map :x (:x state1) :y (int %)))
           (rest))
      (->> (range (:x state1) (:x state2) (Math/signum ^double delta-x))
           (map #(hash-map :x (int %) :y (:y state1)))
           (rest)))))

(defn part2 [input]
  (let [{:keys [state history]}
        (reduce (fn [{:keys [state history]}
                     [side & steps]]
                  (let [steps-val (parse-long (s/join steps))
                        rad-angle (+ (:angle state)
                                     (if (= side \L)
                                       (/ Math/PI 2)
                                       (/ Math/PI -2)))
                        new-state (update-state state rad-angle steps-val)
                        diff (state-difference state new-state)]
                    (let [intersect (cljset/intersection (set history)
                                                         (set (concat diff [(dissoc new-state :angle)])))]
                      (if (seq intersect)
                        (reduced {:state (first intersect) :history history})
                        {:state new-state :history (concat history diff [new-state])}))))
                {:state   {:x 0 :y 0 :angle (/ Math/PI 2)}
                 :history []}
                input)]
    (+ (Math/abs ^double (:x state))
       (Math/abs ^double (:y state)))))