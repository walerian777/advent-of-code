;; https://adventofcode.com/2019/day/6

(use '[clojure.java.io :only (reader)])
(use '[clojure.string :only (split)])

(def input
  (with-open [r (reader "input")]
    (doall (line-seq r))))

(defn prepare-map [raw-map]
  (map #(split % #"\)") raw-map))

(defn graph [orbit-map]
  (into {}
        (map vec
             (map reverse orbit-map))))

(defn count-orbits [object orbit-graph]
  (if (contains? orbit-graph object)
    (+ 1 (count-orbits (orbit-graph object) orbit-graph))
    0))

(defn sum-all-orbits [orbit-graph]
  (reduce + (map #(count-orbits % orbit-graph) (keys orbit-graph))))

(println
  (sum-all-orbits
     (graph
       (prepare-map input))))
