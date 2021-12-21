(ns advent-2021.day21)

(defn deterministic-dice
  ([] (deterministic-dice 1 1))
  ([next round]
   (lazy-seq (cons (list next round) (deterministic-dice (if (= next 100) 1 (inc next)) (inc round))))))

(defn go-forward [pos steps]
  (-> pos
      (dec)
      (+ steps)
      (mod 10)
      (inc)))

(defn do-part1 [input]
  (loop [dice (deterministic-dice)
         pos input
         scores [0 0]]
    (let [p1-roll (reduce + (map first (take 3 dice)))
          first-dice (drop 3 dice)
          first-pos [(go-forward (first pos) p1-roll) (second pos)]
          first-scores [(+ (first scores) (first first-pos)) (second scores)]
          p2-roll (reduce + (map first (take 3 first-dice)))
          second-dice (drop 3 first-dice)
          second-pos [(go-forward (first pos) p1-roll) (go-forward (second pos) p2-roll)]
          second-scores [(+ (first scores) (first second-pos)) (+ (second scores) (second second-pos))]]
      (if (>= (first first-scores) 1000)
        [first-dice first-pos first-scores]
        (if (>= (second second-scores) 1000)
          [second-dice second-pos second-scores]
          (recur second-dice second-pos second-scores))))))

(defn part1 [input]
  (let [[dice _ scores] (do-part1 input)]
    (* (apply min scores) (dec (second (first dice))))))

(def dice-outcomes '{3 1
                     4 3
                     5 6
                     6 7
                     7 6
                     8 3
                     9 1})

(defn solve-2 [pos scores player solve-2*]
  (cond
    (>= (first scores) 21) [1 0]
    (>= (second scores) 21) [0 1]
    :else (reduce (fn [won roll]
                    (let [next-pos (assoc pos player (go-forward (get pos player) roll))
                          next-score (assoc scores player (+ (get scores player) (get next-pos player)))
                          next-wins (mapv #(* (get dice-outcomes roll) %) (solve-2* next-pos next-score (mod (inc player) 2) solve-2*))]
                      (mapv + won next-wins)))
                  [0 0]
                  (keys dice-outcomes))))

(defn part2 [input]
  (let [solve-2* (memoize solve-2)]
    (apply max (solve-2* input [0 0] 0 solve-2*))))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (->> in
                   (mapv #(second (re-seq #"\d+" %)))
                   (mapv #(Integer/parseInt %)))]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))
