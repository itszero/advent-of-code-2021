(ns advent-2021.day11)

(defn do-step [energy-map]
  (let [coords (for [x (range 0 (count energy-map))
                     y (range 0 (count energy-map))]
                 (list y x))
        inc-energy-map (reduce #(assoc-in %1 %2 (inc (get-in %1 %2))) energy-map coords)]
    (loop [energy-map inc-energy-map
           flashes '()]
      (let [handle-flash (fn [[energy-map this-flashes] coord]
                           (let [cur-value (get-in energy-map coord)
                                 seq-plus (fn [a b] (list (+ (first a) (first b)) (+ (second a) (second b))))
                                 safe-inc (fn [coord energy-map]
                                            (let [cur-value (get-in energy-map coord)]
                                              (if (nil? cur-value)
                                                energy-map
                                                (assoc-in energy-map coord (inc cur-value)))))]
                             (if (> cur-value 9)
                               (list (->> energy-map
                                          ((fn [energy-map] (assoc-in energy-map coord 0)))
                                          (safe-inc (seq-plus coord '(-1 -1)))
                                          (safe-inc (seq-plus coord '(-1 0)))
                                          (safe-inc (seq-plus coord '(-1 1)))
                                          (safe-inc (seq-plus coord '(0 -1)))
                                          (safe-inc (seq-plus coord '(0 1)))
                                          (safe-inc (seq-plus coord '(1 -1)))
                                          (safe-inc (seq-plus coord '(1 0)))
                                          (safe-inc (seq-plus coord '(1 1))))
                                     (cons coord this-flashes))
                               (list energy-map this-flashes))))
            [energy-map this-flashes] (reduce handle-flash (list energy-map '()) coords)]
        (if (= (count this-flashes) 0)
          (list (reduce #(assoc-in %1 %2 0) energy-map flashes) (count flashes))
          (recur energy-map (concat flashes this-flashes)))))))

(defn part1 [energy-map]
  (->> (range 1 101)
       (reduce (fn [[energy-map flashes] _]
                 (let [[new-energy-map this-flashes] (do-step energy-map)]
                   (list new-energy-map (+ flashes this-flashes))))
               (list energy-map 0))
       (second)))

(defn part2 [energy-map]
  (loop [energy-map energy-map
         step 1]
    (let [[new-energy-map this-flashes] (do-step energy-map)]
      (if (= this-flashes (* (count energy-map) (count energy-map)))
        step
        (recur new-energy-map (inc step))))))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        energy-map (->> in
                        (map (fn [line] (vec (map #(Integer/parseInt %) (re-seq #"\d" line)))))
                        (vec))]
    (println "part1:" (part1 energy-map))
    (println "part2:" (part2 energy-map))))