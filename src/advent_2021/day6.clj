(ns advent-2021.day6)

(require '[clojure.string :as str])

(defn parse-input [in]
  (let [in-nums (map #(Integer/parseInt %) (str/split in #","))]
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} in-nums)))

(defn process-day [fishes]
  (let [zero-count (get fishes 0 0)]
    (loop [i 1
           fishes fishes]
      (if (= i 9)
        (->> fishes
             (#(assoc % 6 (+ zero-count (get % 6 0))))
             (#(assoc % 8 zero-count)))
        (recur (inc i) (assoc fishes (dec i) (get fishes i 0)))))))

(defn process-days [start-of-day days-left]
  (if (= days-left 0)
    start-of-day
    (let [end-of-day (process-day start-of-day)]
      (recur end-of-day (- days-left 1)))))

(defn part1 [in]
  (let [last-day (process-days in 80)]
    (reduce + (vals last-day))))

(defn part2 [in]
  (let [last-day (process-days in 256)]
    (reduce + (vals last-day))))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (parse-input (first in))]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))