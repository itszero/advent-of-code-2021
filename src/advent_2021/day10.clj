(ns advent-2021.day10)

(require '[advent-2021.utils :refer [debug]])

(defn safe-subs [str idx]
  (if (< (count str) idx)
    ""
    (subs str idx)))

(defn get-score [illegal-character]
  ({")" 3
    "]" 57
    "}" 1197
    ">" 25137} illegal-character))

(def pairs {"(" ")"
            "[" "]"
            "{" "}"
            "<" ">"})
(def opening-chars (keys pairs))
(def closing-chars (vals pairs))

(defn find-first-illegal-character [in]
  (loop [stack []
         in in]
    (let [current-char (if (> (count in) 0) (.toString (first in)) "")]
      (cond
        (= 0 (count in)) (if (= (count stack) 0) (list :valid []) (list :incomplete stack))
        (some? (some #(= current-char %) opening-chars)) (recur (conj stack current-char) (safe-subs in 1))
        (some? (some #(= current-char %) closing-chars)) (if (= (pairs (last stack)) current-char)
                                                           (recur (vec (drop-last stack)) (safe-subs in 1))
                                                           (list :illegal current-char))))))

(defn part1 [in]
  (reduce + (->> in
                 (map find-first-illegal-character)
                 (filter #(= (first %) :illegal))
                 (map second)
                 (map get-score))))

(defn get-closing-score [stack]
  (let [closing-scores {")" 1
                        "]" 2
                        "}" 3
                        ">" 4}
        to-close (->> stack
                      (reverse)
                      (map #(pairs %))
                      (map #(closing-scores %)))]
    (reduce #(+ (* 5 %1) %2) 0 to-close)))

(defn part2 [in]
  (let [closing-scores (->> in
                            (map find-first-illegal-character)
                            (filter #(= (first %) :incomplete))
                            (map second)
                            (map get-closing-score)
                            (sort)
                            (vec))]
    (get closing-scores (quot (count closing-scores) 2))))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))]
    (println "part1:" (part1 in))
    (println "part2:" (part2 in))))