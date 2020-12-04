(ns day04.core
  (:gen-class))

(defn solve [string]
  (->> (clojure.string/split string #"\n.?\n")
       (map (fn [x]
              (->>
               (clojure.string/split x #"(\s|\n|\:)")
               (partition 2)
               (map (fn [[a b]]
                      {a b}))
               (into {}))))))

(defn checkPassport [passport]
  (and (get passport "byr")
       (get passport "iyr")
       (get passport "eyr")
       (get passport "hgt")
       (get passport "hcl")
       (get passport "ecl")
       (get passport "pid")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def passports (-> (slurp "input.txt")
                     (solve)))
  (println (count (filter checkPassport passports))))
