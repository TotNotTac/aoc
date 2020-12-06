(ns day06.core
  (:require [clojure.set]
            [clojure.string])
  (:gen-class))


(defn solve [inputText]
  (->> (clojure.string/split inputText #"\n\n")
       (map (fn [group] (filter #(not= % \newline) group)))
       (map distinct)))

(defn part1 [in]
  (->> (map count in)
       (reduce +)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [solution (->> (slurp "input.txt")
                      (solve))]
    (prn (part1 solution))))
