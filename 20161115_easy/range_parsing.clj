(use '[clojure.string :only [split split-lines join]])

(defn pow10 [i] (reduce * (repeat i 10)))

(defn unshorten [x y]
  (if (> y x)
    y
    (let [l-y (count (str y))
          pred-x (quot x (pow10 l-y))
          last-x (mod x (pow10 l-y))
          pred (if (< last-x y) pred-x (inc pred-x))]
      (Integer. (str pred y)))))

(defn range-part [acc s]
  (let [[t-start t-end t-step] (map #(Integer. %) (split s #"-|:|\.{2}"))
        n (last acc)
        start (if (nil? n) t-start (unshorten n t-start))
        end (inc (if (nil? t-end) start (unshorten start t-end)))
        step (if (nil? t-step) 1 t-step)]
    (into acc (range start end step))))

(defn parse-range [s]
  (println (join " " (reduce range-part [] (split s #",")))))

(seq (map parse-range (split-lines (slurp *in*))))
