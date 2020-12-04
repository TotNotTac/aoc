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

(defn parseHeight [hgtStr]
  (if (< (count hgtStr) 3)
    0
    (do (-> (take (- (count hgtStr) 2) hgtStr)
            (clojure.string/join)
            (read-string)))))

(defn checkPassportPart1 [passport]
  (and (get passport "byr")
       (get passport "iyr")
       (get passport "eyr")
       (get passport "hgt")
       (get passport "hcl")
       (get passport "ecl")
       (get passport "pid")))

(defn checkPassportPart2 [passport]
  (let [byr (get passport "byr")
        iyr (get passport "iyr")
        eyr (get passport "eyr")
        hgtStr (get passport "hgt")
        hgtUnit (reduce str (take-last 2 hgtStr))
        hgt (parseHeight hgtStr)
        hcl (get passport "hcl")
        ecl (get passport "ecl")
        pid (get passport "pid")]

    ;; (print passport)
    (and (= (count byr) 4)
         (>= (read-string byr) 1920)
         (<= (read-string byr) 2002)
         (= (count iyr) 4)
         (>= (read-string iyr) 2010)
         (<= (read-string iyr) 2020)
         (= (count eyr) 4)
         (>= (read-string eyr) 2020)
         (<= (read-string eyr) 2030)
         (cond (= hgtUnit "cm") (and (>= hgt 150)
                                     (<= hgt 193))
               (= hgtUnit "ft") (and (>= hgt 59)
                                     (<= hgt 76)))
         hcl
         (re-matches #"#[a-f0-9]{6}" hcl)
         ecl
         (some #(= % ecl) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
         pid
         (re-matches #"[0-9]{9}" pid)
         )))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def passports (-> (slurp "input.txt")
                     (solve)))
  (println (count (filter checkPassportPart1 passports)))
  (println (count (filter checkPassportPart2 passports))))
