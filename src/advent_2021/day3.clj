(ns advent-2021.day3)

(require '[clojure.string :as str])

(def parse-line
  (fn [str] (map #(Integer/parseInt %) (str/split str #""))))

(def binary-seq-to-int
  (fn [in] (->>
            (reverse in)
            (reduce (fn [data val]
                      (let [base (first data)
                            last-val (second data)]
                        (list (* 2 base) (+ last-val (* base val)))))
                    '(1 0))
            second)))

(def calc-raw
  (fn [in rate-type] (let [lines-count (count in)
                           half-count (/ lines-count 2)
                           count-one (fn [result line] (map #(reduce + %) (map vector result line)))
                           one-count (reduce count-one in)
                           out-binary (map (fn [digit-one-count]
                                             (case rate-type
                                               most (if (>= digit-one-count half-count) 1 0)
                                               least (if (>= digit-one-count half-count) 0 1)))
                                           one-count)]
                       out-binary)))

(def calc (fn [in rate-type] (->> (calc-raw in rate-type)
                                  binary-seq-to-int)))

(def part1
  (fn [in] (let [gamma-rate (calc in 'most)
                 epsilon-rate (calc in 'least)]
             (* gamma-rate epsilon-rate))))

(def find-rating
  (fn [in rate-type digit]
    (if (= (count in) 1)
      (first in)
      (let [calc-out (calc-raw in rate-type)
            filter-goal (nth calc-out digit)
            in-filtered (filter (fn [row] (= filter-goal (nth row digit))) in)]
        (find-rating in-filtered rate-type (+ digit 1))))))

(def part2
  (fn [in] (let [oxygen-generator-rating (find-rating in 'most 0)
                 co2-scrubber-rating (find-rating in 'least 0)]
             (* (binary-seq-to-int oxygen-generator-rating) (binary-seq-to-int co2-scrubber-rating)))))

(def main
  (fn [file] (let [in (line-seq (java.io.BufferedReader. file))
                   lines (map parse-line in)]
               (println "part1:" (part1 lines))
               (println "part2:" (part2 lines)))))