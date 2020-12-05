(ns day05.core
  (:gen-class))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn binaryMultipliers [n]
  (reverse
   (for [i (range n)]
     (exp 2 i))))

(defn convertToBinary [input]
  (->> input
       (map (fn [c]
              (case c
                \F "0"
                \B "1"
                \L "0"
                \R "1")))
       (clojure.string/join)))

(defn rowsAndSeats [[row seat]]
  (-> (* 8 row)
      (+  seat)))

(defn solve [lines]
  (->> (map convertToBinary lines)
       (map (fn [x]
              (->> [(take 7 x) (take-last 3 x)]
                   (map (fn [i]
                          (->> i
                               (map vector (binaryMultipliers (count x)) i)
                               (map (fn [[a b]]
                                      (-> (read-string (str b))
                                          (* a)))))))
                   (map (fn [l] (reduce + l)))
                   (rowsAndSeats))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (solve)
       (apply max)
       (println)))
