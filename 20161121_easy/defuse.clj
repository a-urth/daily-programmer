(ns daily-programmer.20161121_easy.core
  (:use [clojure.string :only [split-lines]]))

(def wire-rules-m {"white" {:allowed #{} :disallowed #{"white" "black"}}
                   "red" {:allowed #{"green"} :disallowed #{}}
                   "black" {:allowed #{} :disallowed #{"white" "green" "orange"}}
                   "orange" {:allowed #{"red" "black"} :disallowed #{}}
                   "green" {:allowed #{"orange" "white"} :disallowed #{}}
                   "purple" {:allowed #{} :disallowed #{"purple" "green" "orange" "white"}}})

(defn check [wire & [{:keys [allowed disallowed]} rules]]
  (and
   (or (empty? allowed) (contains? allowed wire))
   (or (empty? disallowed) (not (contains? disallowed wire)))))

(defn try-defuse
  ([wires]
   (try-defuse wires {:allowed #{}, :disallowed #{}}))

  ([[wire & others] rules]
   (let [cut (check wire rules)]
     (if (nil? others)
       cut
       (and cut (recur others (get wire-rules-m wire)))))))

(println (if (try-defuse (split-lines (slurp *in*))) "Bomb defused" "Boom"))
