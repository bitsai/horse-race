(ns util)

(defn count-if [pred coll]
  (count (filter pred coll)))

(defn find-first [pred coll]
  (first (filter pred coll)))
