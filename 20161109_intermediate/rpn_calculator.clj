(require '[clojure.string :as str])

(def operations {"+" +
                 "-" -
                 "*" *
                 "/" #(float (/ %1 %2))
                 "//" quot
                 "%" mod
                 "^" #(reduce * (repeat %2 %1))
                 "!" #(reduce * (range 1 (inc %1)))})

(def get-args-num #(if (= "!" %) 1 2))

(defn step [stack v]
  (if-let [f (get operations v)]
    (let [n (get-args-num v)]
      (if (< (count stack) n)
        (throw (Exception. "Invalid equation"))
        (let [split-point (- (count stack) n)
              [new-stask args] (split-at split-point stack)]
          (conj (into [] new-stask) (apply f args)))))
    (conj stack (read-string v))))

(defn calculate [rpn]
  (println (first (reduce step [] (str/split rpn #" ")))))

(seq (map calculate (str/split-lines (slurp "input"))))
