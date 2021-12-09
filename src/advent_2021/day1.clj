(ns advent-2021.day1)

(def part1
  (fn [in] (let [proc (fn [data val]
                        (let [last-count (first data)
                              last-val (second data)
                              new-count (if (< last-val val)
                                          (+ last-count 1)
                                          last-count)]
                          (list new-count val)))
                 out (first (reduce proc (list 0 (first in)) in))]
             out)))

(def part2
  (fn [in] (let [proc (fn [data val]
                        (let [last-count (first data)
                              last-window (second data)
                              new-window (if (< (count last-window) 3)
                                           (conj last-window val)
                                           (conj (vec (drop 1 last-window)) val))
                              sum (fn [col] (reduce + col))
                              increased (if (= (count last-window) 3)
                                          (< (sum last-window) (sum new-window))
                                          false)
                              new-count (if increased
                                          (+ last-count 1)
                                          last-count)]
                          (list new-count new-window)))
                 out (first (reduce proc (list 0 []) in))]
             out)))
(def main
  (fn [file] (let [in (map #(Integer/parseInt %) (line-seq (java.io.BufferedReader. file)))]
               (println "part1:" (part1 in))
               (println "part2:" (part2 in)))))