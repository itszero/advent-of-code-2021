(ns advent-2021.day8)

(require '[clojure.string :as str])
(require '[advent-2021.utils :refer [map-map-values find-map-by-val]])

(def correct-patterns
  (map-map-values seq
                  {0 "abcefg"
                   1 "cf"
                   2 "acdeg"
                   3 "acdfg"
                   4 "bcdf"
                   5 "abdfg"
                   6 "abdefg"
                   7 "acf"
                   8 "abcdefg"
                   9 "abcdfg"}))

(defrecord Input [patterns output])
(defn sort-wires [wires]
  (->> wires
       (seq)
       (sort)
       (str/join)))
(defn parse-input [line]
  (let [parts (str/split line #" \| ")
        patterns (map sort-wires (str/split (first parts) #" "))
        output (map sort-wires (str/split (second parts) #" "))]
    (Input. patterns output)))

(defn part1 [in]
  (let [is-easy-digit (fn [wires] (case (count wires)
                                    2 1
                                    4 1
                                    3 1
                                    7 1
                                    0))]
    (->> in
         (map #(->> %
                    (:output)
                    (map is-easy-digit)
                    (reduce +)))
         (reduce +))))

(defn sort-patterns [patterns]
  (let [digit-1 (first (filter #(= (count %) 2) patterns))
        digit-4 (first (filter #(= (count %) 4) patterns))
        digit-7 (first (filter #(= (count %) 3) patterns))
        digit-8 (first (filter #(= (count %) 7) patterns))]
    (concat (list digit-1 digit-7 digit-4 digit-8)
            (filter #(and (not= % digit-1) (not= % digit-4) (not= % digit-7) (not= % digit-8)) patterns))))

(defn make-mapping [to from & [mapping]]
  (let [mapping (if (nil? mapping) {} mapping)]
    (if (= (count to) 0) mapping
        (flatten (reduce (fn [out from-val]
                           (let [next-to (rest to)
                                 next-from (filter (complement #{from-val}) from)
                                 next-mapping (assoc mapping from-val (first to))
                                 answers (make-mapping next-to next-from next-mapping)]
                             (cons answers out))) nil from)))))

(defn restore-wires [mapping pattern]
  (let [restore-one-wire (fn [wire] (find-map-by-val mapping wire))]
    (->> pattern
         (seq)
         (map restore-one-wire)
         (str/join)
         (sort-wires)
         (seq))))

(defn solve-patterns [patterns & [full-patterns mapping]]
  (let [filter-solved-from-wires-fn (fn [wires] (filter #(nil? (get mapping %)) wires))
        filter-solved-to-wires-fn (fn [wires] (filter (complement #(some #{%} (vals mapping))) wires))
        mapping (if (nil? mapping) {} mapping)
        full-patterns (if (nil? full-patterns) patterns full-patterns)
        to-solve (first patterns)
        rest-patterns (rest patterns)
        to-solve-unsolved (filter-solved-to-wires-fn (seq to-solve))
        possible-matches (filter #(= (count %) (count to-solve)) (vals correct-patterns))
        possible-matches-unsolved (filter #(> (count %) 0) (map #(filter-solved-from-wires-fn %) possible-matches))
        possible-matches-final (filter #(= (count %) (count to-solve-unsolved)) possible-matches-unsolved)]
    (cond
      (= (count patterns) 0) (if (every?
                                  (fn [pattern]
                                    (let [restored-pattern (restore-wires mapping pattern)]
                                      (some #{restored-pattern} (vals correct-patterns))))
                                  full-patterns)
                               mapping
                               nil)
      (= (count to-solve-unsolved) 0) (solve-patterns rest-patterns full-patterns mapping)
      (= (count possible-matches-final) 0) nil
      :else (reduce
             (fn [_ val]
               (let [seq-to-test (make-mapping to-solve-unsolved val)]
                 (reduce (fn [_ seq-to-add]
                           (let [new-mapping (reduce #(assoc %1 (first %2) (second %2)) mapping seq-to-add)]
                             (if-let [new-mapping (solve-patterns rest-patterns full-patterns new-mapping)]
                               (reduced new-mapping)
                               nil)))
                         nil seq-to-test)))
             nil possible-matches-final))))

(defn part2 [in]
  (->> in
       (map (fn [line]
              (let [mapping (solve-patterns (sort-patterns (:patterns line)))
                    digits (map #(->> %
                                      (restore-wires mapping)
                                      (find-map-by-val correct-patterns)) (:output line))
                    output-number (first (reduce
                                          (fn [[out base] digit] (list (+ out (* base digit)) (* base 10)))
                                          '(0 1)
                                          (reverse digits)))]
                output-number)))
       (reduce +)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (map parse-input in)]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))