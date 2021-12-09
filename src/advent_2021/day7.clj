(ns advent-2021.day7)

(defn part1 [in]
  (let [min-x (apply min in)
        max-x (apply max in)
        x-range (range min-x (+ 1 max-x))
        calc-total-fuel (fn [target-x] (->> in
                                            (map #(Math/abs (- % target-x)))
                                            (reduce +)))
        min-fuel-pos (apply min-key calc-total-fuel x-range)]
    (calc-total-fuel min-fuel-pos)))

(defn part2 [in]
  (let [min-x (apply min in)
        max-x (apply max in)
        x-range (range min-x (+ 1 max-x))
        calc-fuel (fn [steps] (reduce + (range 1 (+ 1 steps))))
        calc-total-fuel (fn [target-x] (->> in
                                            (map #(calc-fuel (Math/abs (- % target-x))))
                                            (reduce +)))
        min-fuel-pos (apply min-key calc-total-fuel x-range)]
    (calc-total-fuel min-fuel-pos)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (map #(Integer/parseInt %) (re-seq #"\d+" (first in)))]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))