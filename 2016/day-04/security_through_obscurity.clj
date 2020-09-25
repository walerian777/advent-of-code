;; https://adventofcode.com/2016/day/4

(use '[clojure.string :only (join split starts-with?)])

(def input
  (split (slurp "input") #"\n"))

(defn room [code]
  (let [[_ name id checksum]
        (re-matches #"(.+)-(\d+)\[(.*)\]" code)]
    (fn [message]
      (case message
        :name (clojure.string/replace name #"-" "")
        :id (Integer/parseInt id)
        :checksum checksum))))

(def rooms
  (map room input))

(defn calculate-checksum [room]
  (join (map first (take 5 (sort-by val > (into (sorted-map) (frequencies (room :name))))))))

(defn real-room? [room]
  (= (calculate-checksum room) (room :checksum)))

(println
 (apply + (keep #(if (real-room? %) (% :id)) rooms)))

(def x (room "qzmt-zixmtkozy-ivhz-343[abxyz]"))

(def alphabet
  (seq "abcdefghijklmnopqrstuvwxyz"))

(defn cipher [shift]
  (->> (cycle alphabet) (drop shift) (take 26) (zipmap alphabet)))

(defn decrypt [room]
  (apply str (replace (cipher (room :id)) (room :name))))

(def decrypted-rooms
  (map #(list (decrypt %) (% :id)) rooms))

(println
 (filter #(starts-with? (first %) "northpole") decrypted-rooms))
