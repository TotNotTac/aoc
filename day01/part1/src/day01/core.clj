(ns day01.core
  (:gen-class))

(defn solve
  "Solve the program"
  [inputList]

  (nth (for [x inputList
             y inputList
             :when (= (+ x y) 2020)]
         (* x y))
       0))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def lines (->> (clojure.string/split-lines (slurp "input.txt"))
                  (map read-string)))
  (println (solve lines)))
