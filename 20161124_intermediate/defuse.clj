(ns daily-programmer.20161124_intermediate.core
  (:use [clojure.string :only [split split-lines]]))

(def wire-rules {:s0 {\w :s1, \r :s2}
                 :s1 {\w :s2, \o :s3}
                 :s2 {\b :s3, \r :s0}
                 :s3 {\b :s3, \o :s4, \g :s5}
                 :s4 {\g :exit}
                 :s5 {\o :exit}})

(defn end? [kw] (= kw :exit))

(defn defuse
  ([wires]
   (defuse wires :s0))

  ([[wire & others] cur-step]
   (let [next-step ((wire-rules cur-step) wire)]
     (condp = next-step
       :exit true
       nil false
       (recur others next-step)))))

(defn defuse-bomb [i wires]
  (println
   (if (defuse (map first (split-lines wires)))
     (format "%s - Bomb defused" i)
     (format "%s - Booom" i))))

(defn wires->map [wires]
  (into {} (for [wire-row (split-lines wires)
                 :let [[wire n] (split wire-row #" ")]]
             [(first wire) (Integer. n)])))

(defn no-wires-left? [wires-m] (every? zero? (vals wires-m)))

(defn defusable?
  ([wires-m]
   (defusable? wires-m :s0))

  ([wires-m step]
   (if (end? step)
     (no-wires-left? wires-m)
     (loop [[[wire next-step] & other] (seq (wire-rules step))]
       (if (nil? wire)
         false
         (let [wire-n (wires-m wire)]
           (if (and
                (not (zero? wire-n))
                (defusable? (assoc wires-m wire (dec wire-n)) next-step))
             true
             (recur other))))))))

(defn check-bomb [i wires]
  (println
   (if (defusable? (wires->map wires))
     (format "%s - Defusable" i)
     (format "%s - Not defusable" i))))

(time
  (doall
    (map-indexed
     (if (= (second *command-line-args*) "--check") check-bomb defuse-bomb)
     (split (slurp *in*) #"\n\n"))))
