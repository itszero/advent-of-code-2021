(ns advent-2021.day14)

(require '[clojure.string :as str])
(require '[advent-2021.utils :refer [debug map-map-values]])

(defrecord Input [initial rules])
(defn parse-input [in]
  (let [initial (first in)
        raw-rules (drop 2 in)
        rules (map #(rest (re-matches #"([A-Z]+) -> ([A-Z])" %)) raw-rules)]
    (Input. initial (apply hash-map (mapcat #(list (seq (first %)) (first (second %))) rules)))))

(defn run-step [polymer rules]
  (loop [polymer polymer
         inserted-polymer '[]]
    (let [last-element (last inserted-polymer)
          next (first polymer)]
      (cond
        (nil? next) (str/join inserted-polymer)
        (nil? last-element) (recur (rest polymer) (conj inserted-polymer next))
        :else (recur
               (rest polymer)
               (if-let [to-insert (get rules (str/join (list last-element next)))]
                 (-> inserted-polymer
                     (conj to-insert)
                     (conj next))
                 (conj inserted-polymer next)))))))

(defn polymer-to-pairs [polymer]
  (->> polymer
       (seq)
       (partition 2 1)
       (group-by identity)
       (map-map-values #(count %))))

(defn run-step-pairs [pairs rules]
  (reduce (fn [new-pairs pair]
            (if-let [to-insert (get rules pair)]
              (let [first-pair (list (first pair) to-insert)
                    second-pair (list to-insert (second pair))]
                (-> new-pairs
                    (assoc first-pair (+ (get new-pairs first-pair 0) (get pairs pair)))
                    (assoc second-pair (+ (get new-pairs second-pair 0) (get pairs pair)))))
              (assoc new-pairs pair (+ (get new-pairs pair 0) (get pairs pair)))))
          '{}
          (keys pairs)))

(defn count-elements [pairs last-char]
  (let [counted (->> pairs
                     (keys)
                     (reduce (fn [counted key]
                               (let [first-element (first key)]
                                 (-> counted
                                     (assoc first-element (+ (get counted first-element 0) (get pairs key))))))
                             '{}))]
    (assoc counted last-char (inc (get counted last-char 0)))))

(defn part1 [input]
  (let [polymer-10-steps (reduce (fn [pairs _] (run-step-pairs pairs (:rules input))) (polymer-to-pairs (:initial input)) (range 0 10))
        counted (count-elements polymer-10-steps (last (:initial input)))
        buckets (vals counted)]
    (- (apply max buckets) (apply min buckets))))

(defn part2 [input]
  (let [polymer-40-steps (reduce (fn [pairs _] (run-step-pairs pairs (:rules input))) (polymer-to-pairs (:initial input)) (range 0 40))
        counted (count-elements polymer-40-steps (last (:initial input)))
        buckets (vals counted)]
    (- (apply max buckets) (apply min buckets))))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (parse-input in)]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))
