(ns day7.core
  (:gen-class))

(def input (slurp "input.txt"))

(def testInput "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")


(def rules (->> (clojure.string/replace input #"bags?\.?" "")
                (clojure.string/split-lines)
                (map #(clojure.string/split % #" contain "))
                (map (fn [rule]
                       [(clojure.string/trim (first rule))
                        (map (comp #(map (comp (partial clojure.string/trim) (partial apply str)) %)
                                   (partial reverse)
                                   (partial split-at 2)
                                   #(apply str %))
                             (clojure.string/split (second rule) #" , "))]))
                (into {})))

(defn findBagRecursive [rules bagToFind searchBag]
  (let [bags (map first (get rules searchBag))
        results (or (some #{bagToFind} bags)
                    (->> (map (partial findBagRecursive rules bagToFind) bags)
                         (filter #(not (empty? %)))
                         ))]
    (if (not (empty? results))
      searchBag
      nil
      )))

(prn (->> (map (partial first) rules)
          (map (partial findBagRecursive rules "shiny gold"))
          (filter some?)
          (set)
          (count)))
(prn rules)
