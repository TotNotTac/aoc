(ns day08.core
  (:gen-class))

;; (defn -main [& args]
;;   )

(def testInput "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def input (->> (re-seq #"(\w{3}) ([+-]\d+)" (slurp "input.txt"))
                (map (fn [[_ k v]]
                       [(keyword k) v]))
                (vec)
                (map (fn [[k v]]
                       [k
                        (read-string v)]))))

(defn processRecur [input & {:keys [acc pc seen?]
                             :or {acc 0 pc 0 seen? #{}}}]
  (if (seen? pc)
    acc
    (let [k (first (nth input pc))
          a (second (nth input pc))]
      (case k
        :jmp (processRecur input :acc acc :pc (+ pc a) :seen? (conj seen? pc))
        :nop (processRecur input :acc acc :pc (inc pc) :seen? (conj seen? pc))
        :acc (processRecur input :acc (+ acc a) :pc (inc pc) :seen? (conj seen? pc))))))

(processRecur input)
