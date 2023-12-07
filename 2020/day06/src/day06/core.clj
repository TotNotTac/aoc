(ns day06.core
  (:require [clojure.set]
            [clojure.string])
  (:gen-class))


(defn solve [inputText]
  (clojure.string/split inputText #"\n\n"))

(defn part1 [in]
  (->> in
       (map (fn [group] (filter #(not= % \newline) group)))
       (map distinct)
       (map count)
       (reduce +)))

(defn count-occurrences [s slist]
  (->> slist
       flatten
       (filter #{s})
       count))

(defn part2 [in]
  (let [entries (map (fn [group] (filter #(not= % \newline) group)) in)]
    (->> entries
         (map (fn [entry]
                (->> (let [peopleCount (count (clojure.string/split-lines (str entry)))]
                       (for [c (distinct entry)]
                         (if (= peopleCount (count-occurrences c entry))
                           1 0)))
                     (reduce +))))
         (reduce +))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [solution (->> (slurp "testInput.txt")
                      (solve))]
    (prn (part1 solution))
    (prn (part2 solution))))
