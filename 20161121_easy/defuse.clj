(ns daily-programmer.20161121_easy.core
  (:use [clojure.string :only [split-lines]]))

(def wire-rules-m {"white" {:to-cut #{} :not-to-cut #{"white" "black"}}
                   "red" {:to-cut #{"green"} :not-to-cut #{}}
                   "black" {:to-cut #{} :not-to-cut #{"white" "green" "orange"}}
                   "orange" {:to-cut #{"red" "black"} :not-to-cut #{}}
                   "green" {:to-cut #{"orange" "white"} :not-to-cut #{}}
                   "purple" {:to-cut #{} :not-to-cut #{"purple" "green" "orange" "white"}}})

(defn can-cut? [wire & [{:keys [to-cut not-to-cut]} rules]]
  (and
   (or (empty? to-cut) (contains? to-cut wire))
   (or (empty? not-to-cut) (not (contains? not-to-cut wire)))))

(defn defuse
  ([wires]
   (defuse wires {:to-cut #{}, :not-to-cut #{}}))

  ([[wire & others] rules]
   (let [cut (can-cut? wire rules)]
     (if (nil? others)
       cut
       (and cut (recur others (wire-rules-m wire)))))))

(println (if (defuse (split-lines (slurp *in*))) "Bomb defused" "Boom"))
