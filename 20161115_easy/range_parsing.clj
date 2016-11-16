(use '[clojure.string :only [split split-lines join]])

(defn unshorten [x y]
  (let [iy (Integer. y)
        ix (Integer. x)
        l-delta (- (count (str x)) (count (str y)))]
    (if (< iy ix)
      (loop [ty (+ iy (reduce * (repeat l-delta 10)))
             tx ix]
        (if (and (> ty tx) (.endsWith (str ty) y))
          ty
          (recur (inc ty) tx)))
      iy)))

(defn range-part [acc s]
  (let [[t-start t-end t-step] (split s #"-|:|\.{2}")
        n (last acc)
        start (if (nil? n) (Integer. t-start) (unshorten n t-start))
        end (inc (if (nil? t-end) start (unshorten start t-end)))
        step (if (nil? t-step) 1 (Integer. t-step))]
    (into acc (range start end step))))

(defn parse-range [s]
  (println (join " " (reduce range-part [] (split s #",")))))

(seq (map parse-range (split-lines (slurp *in*))))
