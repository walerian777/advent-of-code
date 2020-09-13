;; https://adventofcode.com/2016/day/3

(use '[clojure.string :only (split blank?)])

(def input
  (map #(split % #" ") (split (slurp "input") #"\n")))

(def triangles
  (map
   (fn [sides]
     (keep #(if (not (blank? %)) (Integer/parseInt %)) sides)) input))

(defn valid-triagle? [a b c]
  (and
   (> (+ a b) c)
   (> (+ a c) b)
   (> (+ b c) a)))

(println
 (count (filter #(apply valid-triagle? %) triangles)))
