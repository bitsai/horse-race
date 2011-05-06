(ns util)

(defn count-if [pred coll]
  (count (filter pred coll)))
