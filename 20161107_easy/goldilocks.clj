(require '[clojure.string :as str])

(defn to-pair-int [pair-str]
  (map #(Integer/parseInt %) (str/split pair-str #" ")))

(defn read-input [filename]
  (map
   to-pair-int
   (str/split-lines (slurp filename))))

(defn suitable? [[g-weight g-temp] [c-weight c-temp]]
  (and (>= c-weight g-weight) (<= c-temp g-temp)))

(defn find-suitable [[goldi & other]]
  (keep-indexed #(if (suitable? goldi %2) (inc %1)) other))

(println (find-suitable (read-input "input")))
