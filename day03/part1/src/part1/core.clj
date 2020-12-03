(ns part1.core
  (:gen-class))

(defn getPos
  [x y arr]
  (get-in arr [x y]))

(defn generateCoords
  ([]
   (generateCoords 1 3))
  ([x y]
   (lazy-seq
    (cons [x y] (generateCoords (+ 3 x) (+ 1 y))))))

(defn solve
  [arr]

  (println arr)
  (println "test")
  (take-while (fn [x]
                (println x)
                (and (<= (nth x 0)
                         25)
                     (<= (nth x 1)
                         25)))
              (generateCoords)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def lines (clojure.string/split-lines (slurp "input.txt")))
  (solve lines))
