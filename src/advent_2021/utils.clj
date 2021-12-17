(ns advent-2021.utils)

(defn debug [name val]
  (println name val)
  val)

(defn max-by [f coll]
  (reduce #(if (> (f %2) (f %1)) %2 %1) coll))

(defn find-map-by-val [hm val]
  (->> (keys hm)
       (filter #(= val (hm %)))
       (first)))

; utility code from internet
(defn map-map
  [f m]
  (into (empty m) (map #(apply f %) m)))

(defn map-map-values [f m]
  (map-map (fn [key value] {key (f value)}) m))

