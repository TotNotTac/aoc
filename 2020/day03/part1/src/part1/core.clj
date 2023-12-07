(ns part1.core
  (:gen-class))

(defn getPos
  [x y arr]
  (def line (get arr y))
  (nth (cycle line) x))

(defn solve
  [arr xStep yStep]
  (def x 0)
  (def y 0)
  (def treeCount 0)

  (while (< y (count arr))
    (when (= \#
             (getPos x y arr))
      (def treeCount (inc treeCount)))

    (def x (+ xStep x))
    (def y (+ yStep y)))
  treeCount)

(defn -main
  ;; "I don't do a whole lot ... yet."
  [& args]
  (def lines (clojure.string/split-lines (slurp "input.txt")))
  (println (solve lines 3 1))
  (println (* (solve lines 1 1)
              (solve lines 3 1)
              (solve lines 5 1)
              (solve lines 7 1)
              (solve lines 1 2))))
