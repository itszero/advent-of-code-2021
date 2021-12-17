(ns advent-2021.day15)

(require '[advent-2021.utils :refer [debug]])

(def directions
  '[(-1 0)
    (0 -1)
    (0 1)
    (1 0)])

(defn add-point [a b]
  (list (+ (first a) (first b))
        (+ (second a) (second b))))

(defn is-valid-point [risk-map [y x]]
  (and (>= x 0) (>= y 0) (< y (count risk-map)) (< x (count (first risk-map)))))

(defn calc-distance-map [risk-map start]
  (loop [distance-map (vec (repeat (count risk-map) (vec (repeat (count (first risk-map)) nil))))
         queue [(list start 0)]]
    (if (= (count queue) 0)
      distance-map
      (let [[point distance] (first queue)]
        (if (nil? (get-in distance-map point nil))
          (let [new-points (map #(add-point point %) directions)
                new-queue (reduce (fn [queue new-point]
                                    (if (is-valid-point risk-map new-point)
                                      (conj queue
                                            (list new-point (+ distance (get-in risk-map new-point))))
                                      queue))
                                  (vec (rest queue))
                                  new-points)]
            (recur
             (assoc-in distance-map point distance)
             (sort-by second new-queue)))
          (recur distance-map (rest queue)))))))

(defn part1 [risk-map]
  (let [distance-map (calc-distance-map risk-map '(0 0))]
    (get-in distance-map (list (dec (count risk-map)) (dec (count (first risk-map)))))))

(defn inc-map [risk-map]
  (vec (map (fn [row] (vec (map #(if (= % 9)
                                   1
                                   (inc %)) row)))
            risk-map)))

(defn concat-right [risk-map right-map]
  (loop [risk-map risk-map
         right-map right-map
         new-map '[]]
    (let [risk-row (first risk-map)
          right-row (first right-map)]
      (cond
        (and (nil? risk-row) (nil? right-row)) new-map
        (and (some? risk-row) (some? right-row)) (recur
                                                  (vec (rest risk-map))
                                                  (vec (rest right-map))
                                                  (conj new-map (vec (concat risk-row right-row))))
        :else (throw "map not concat-able")))))

(defn concat-down [risk-map down-map]
  (reduce #(conj %1 %2) risk-map down-map))

(defn part2 [risk-map]
  (let [right-5-map (first (reduce (fn [[out-map last-inc-map] _]
                                     (list (concat-right out-map (inc-map last-inc-map))
                                           (inc-map last-inc-map)))
                                   (list risk-map risk-map)
                                   (range 0 4)))
        new-risk-map (first (reduce (fn [[out-map last-inc-map] _]
                                      (list (concat-down out-map (inc-map last-inc-map))
                                            (inc-map last-inc-map)))
                                    (list right-5-map right-5-map)
                                    (range 0 4)))
        distance-map (calc-distance-map new-risk-map '(0 0))]
    (let []
      (get-in distance-map (list (dec (count new-risk-map)) (dec (count (first new-risk-map))))))))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        risk-map (vec (map (fn [line] (->> line
                                           (re-seq #"\d")
                                           (map #(Integer/parseInt %))
                                           (vec))) in))]
    (println "part1:" (part1 risk-map))
    (println "part2:" (part2 risk-map))))
