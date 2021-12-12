(ns advent-2021.day12)

(require '[clojure.string :as str])

(defn parse-input [in]
  (let [pairs (map #(str/split % #"-") in)]
    (reduce #(->> %1
                  ((fn [graph] (assoc graph (first %2) (conj (get graph (first %2) []) (second %2)))))
                  ((fn [graph] (assoc graph (second %2) (conj (get graph (second %2) []) (first %2))))))
            '{}
            pairs)))

(defn find-all-paths
  ([graph] (find-all-paths graph '["start"]))
  ([graph stack]
   (let [start (if-let [last-stack (last stack)] last-stack "start")
         next (->> (get graph start '())
                   (filter (fn [x] (if (Character/isLowerCase (get x 0))
                                     (not (some #{x} stack))
                                     true)))
                   (filter (complement #{"start"})))]
     (if (= start "end")
       (list stack)
       (reduce #(concat %1 (find-all-paths graph (conj stack %2))) '() next)))))

(defn part1 [graph]
  (count (find-all-paths graph)))

(defn find-all-paths-2
  ; this is terribly inefficient (took 2.11h to get day2 answer)
  ([graph] (find-all-paths-2 graph (map #(list (vector "start" %)
                                               (set (list "start" %)) false)
                                        (get graph "start" '()))))
  ([graph queue]
   (loop [queue queue
          all-paths '[]]
     (if (= (count queue) 0)
       all-paths
       (let [[stack visited has-two] (first queue)
             next-queue (rest queue)
             start (if-let [last-stack (last stack)] last-stack "start")
             next (->> (get graph start '())
                       (filter (fn [x] (if (and (Character/isLowerCase (get x 0)) (contains? visited x))
                                         (not has-two)
                                         true)))
                       (filter (complement #{"start"})))]

         (if (= start "end")
           (recur next-queue (conj all-paths stack))
           (recur (concat next-queue (map #(list (conj stack %)
                                                 (conj visited %)
                                                 (if (Character/isLowerCase (get % 0))
                                                   (or has-two (contains? visited %))
                                                   has-two))
                                          next))
                  all-paths)))))))

(defn part2 [graph]
  (count (find-all-paths-2 graph)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        graph (parse-input in)]
    (println "part1:" (part1 graph))
    (println "part2:" (part2 graph))))