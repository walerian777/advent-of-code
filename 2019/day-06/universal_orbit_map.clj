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
  ;; Part 1
  (sum-all-orbits
     (graph
       (prepare-map input))))

(defn lazy-contains? [collection key]
    (some #{key} collection))

(defn traverse-orbits [object orbit-graph]
  (if (contains? orbit-graph object)
    (concat [object] (traverse-orbits (orbit-graph object) orbit-graph))
    []))

(defn orbit-travels-to-santa [orbit-graph]
  (concat
    (remove #(lazy-contains? (traverse-orbits "SAN" orbit-graph) %)
            (traverse-orbits "YOU" orbit-graph))
    (remove #(lazy-contains? (traverse-orbits "YOU" orbit-graph) %)
            (traverse-orbits "SAN" orbit-graph))
    ))

(println
  ;; Part 2
  (count
    (orbit-travels-to-santa
      (graph
        (prepare-map input)))))
