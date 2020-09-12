;; https://adventofcode.com/2016/day/1

(use '[clojure.string :only (split)])

(def input
  (slurp "input"))

(def instructions
  (map #(split % #"") (split input #", ")))

(defn point [x y]
  (fn [message]
    (case message
      :x x
      :y y)))

(defn location [direction point]
  (fn [message]
    (case message
      :direction direction
      :point point)))

(def starting-point
  (location "N" (point 0 0)))

(defn move [current-location turn distance]
  (let [x ((current-location :point) :x)
        y ((current-location :point) :y)]
    (case (current-location :direction)
      "N" (if (= turn "R")
            (location "E" (point (+ x distance) y))
            (location "W" (point (- x distance) y)))
      "E" (if (= turn "R")
            (location "S" (point x (- y distance)))
            (location "N" (point x (+ y distance))))
      "S" (if (= turn "R")
            (location "W" (point (- x distance) y))
            (location "E" (point (+ x distance) y)))
      "W" (if (= turn "R")
            (location "N" (point x (+ y distance)))
            (location "S" (point x (- y distance)))))))

(defn find-destination []
  (reduce
   (fn [acc instruction]
     (println [
               ((acc :point) :x)
               ((acc :point) :y)
               ])
     (move acc (first instruction) (Integer/parseInt (second instruction))))
   starting-point
   instructions))

(defn manhattan-distance [p1 p2]
  (let [x1 (p1 :x) y1 (p1 :y) x2 (p2 :x) y2 (p2 :y)]
    (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(println
 (manhattan-distance (starting-point :point) ((find-destination) :point)))
