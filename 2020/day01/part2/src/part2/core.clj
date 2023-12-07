(ns part2.core
  (:gen-class))

(defn solve
  "Solve the program"
  [inputList]

  (nth (for [x inputList
             y inputList
             z inputList
             :when (= (+ x y z) 2020)]
         (* x y z))
       0))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def lines (->> (clojure.string/split-lines (slurp "input.txt"))
                  (map read-string)))
  (println (solve lines)))
