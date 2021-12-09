(ns advent-2021.day9)

(defn get-by-xy [height-map x y]
  (if-let [row (nth height-map y nil)]
    (if-let [data (nth row x nil)]
      data
      nil)
    nil))

(defn check-neighbors-higher [height-map x y]
  (let [to-compare (get-by-xy height-map x y)
        top (get-by-xy height-map x (- y 1))
        bottom (get-by-xy height-map x (+ y 1))
        left (get-by-xy height-map (- x 1) y)
        right (get-by-xy height-map (+ x 1) y)]
    (and (or (nil? top) (> top to-compare))
         (or (nil? bottom) (> bottom to-compare))
         (or (nil? left) (> left to-compare))
         (or (nil? right) (> right to-compare)))))

(defn part1 [height-map]
  (let [width (count (first height-map))
        height (count height-map)
        range-w (range 0 width)
        range-h (range 0 height)
        risk-map (for [x range-w
                       y range-h]
                   (if (check-neighbors-higher height-map x y)
                     (inc (get-by-xy height-map x y))
                     0))]
    (reduce + (flatten risk-map))))

(defn flood-fill
  ([height-map] (flood-fill height-map '()))
  ([height-map basin-sizes]
   (let [width (count (first height-map))
         height (count height-map)
         range-w (range 0 width)
         range-h (range 0 height)
         flood-at (first (for [x range-w
                               y range-h
                               :when (not= 9 (get-by-xy height-map x y))]
                           (list x y)))]
     (cond
       (nil? flood-at) basin-sizes
       :else (let [[new-height-map fill-size] (apply flood-fill height-map flood-at)]
               (flood-fill new-height-map (cons fill-size basin-sizes))))))
  ([height-map x y]
   (let [val (get-by-xy height-map x y)]
     (if (or (nil? val) (= val 9))
       [height-map 0]
       (reduce (fn [[height-map fill-size] [dx dy]]
                 (let [nx (+ x dx)
                       ny (+ y dy)
                       new-height-map (assoc-in height-map [y x] 9)
                       [recur-height-map recur-fill-size] (flood-fill new-height-map nx ny)]
                   [recur-height-map (+ fill-size recur-fill-size)]))
               [height-map 1]
               '((0 1) (0 -1) (1 0) (-1 0)))))))

(defn part2 [height-map]
  (let [basin-sizes (flood-fill height-map)]
    (->> basin-sizes
         (sort)
         (take-last 3)
         (reduce *))))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        height-map (vec (map (fn [line] (vec (map #(Integer/parseInt %) (re-seq #"\d" line)))) in))]
    (println "part1:" (part1 height-map))
    (println "part2:" (part2 height-map))))