(ns util)

(defn any? [pred coll]
  (if (some pred coll)
    true
    false))

(defn count-if [pred coll]
  (count (filter pred coll)))
