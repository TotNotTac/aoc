(ns day05.core
  (:require [clojure.set :as s])
  (:gen-class))


(defn exp [x n]
  (reduce * (repeat n x)))

(defn binaryMultipliers [n]
  (reverse (for [i (range n)]
             (exp 2 i))))

(defn convertToNumber [x]
  (->> (map vector x (binaryMultipliers (count x)))
       (map (fn [i]
              (reduce * i)))
       (reduce +)))

(defn combineNumbers [[a b]]
  (+ b
     (* a 8)))

(defn solve [lines]
  (->> lines
       (map (fn [line]
              (->> [(take 7 line) (take-last 3 line)]
                   (map (fn [numberString]
                          (->> numberString
                               (map (fn [c]
                                      (case c
                                        \F 0
                                        \B 1
                                        \R 1
                                        \L 0)))
                               (convertToNumber))))
                   (combineNumbers))))))

(defn part1 [solution]
  (->> solution
       (apply max)))

(defn part2 [solution]
  (s/difference (set (range 1024))
                (set solution)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [solution (->> (slurp "input.txt")
                      (clojure.string/split-lines)
                      (solve)
                      )]
    (println (part1 solution))
    (println (part2 solution))
    ))
