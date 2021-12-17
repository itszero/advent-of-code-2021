(ns advent-2021.day17)

(require '[advent-2021.utils :refer [max-by]])

(defn is-in-target [x1 x2 y1 y2 x y]
  (and (<= x1 x) (<= x x2)
       (<= y1 y) (<= y y2)))

(defn calc-max-y [x1 x2 y1 y2 xv yv]
  (loop [x 0
         y 0
         xv xv
         yv yv
         max-y nil]
    (cond
      (is-in-target x1 x2 y1 y2 x y) max-y
      (or (and (= xv 0) (or (< x x1) (> x x2))) (< y y1)) nil
      :else (recur (+ x xv)
                   (+ y yv)
                   (cond
                     (> xv 0) (dec xv)
                     (< xv 0) (inc xv)
                     (= xv 0) 0)
                   (dec yv)
                   (if (nil? max-y) (+ y yv) (max (+ y yv) max-y))))))

(defn part1 [x1 x2 y1 y2]
  (->> (for [xv (range 1 (inc x2))
             yv (range y1 (inc (- y1)))]
         (list xv yv (calc-max-y x1 x2 y1 y2 xv yv)))
       (filter (complement #(nil? (nth % 2))))
       (max-by #(nth % 2))
       (last)))

(defn part2 [x1 x2 y1 y2]
  (->> (for [xv (range 1 (inc x2))
             yv (range y1 (inc (- y1)))]
         (list xv yv (calc-max-y x1 x2 y1 y2 xv yv)))
       (filter (complement #(nil? (nth % 2))))
       (count)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (map #(Integer/parseInt %) (re-seq #"-?\d+" (first in)))]
    (println "part1:" (apply part1 input))
    (println "part2:" (apply part2 input))))
