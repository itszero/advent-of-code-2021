(ns advent-2021.day22)

(require '[clojure.string :as str])
(require '[advent-2021.utils :refer [debug]])

(defn intersect? [[c1x1 c1x2 c1y1 c1y2 c1z1 c1z2] [c2x1 c2x2 c2y1 c2y2 c2z1 c2z2]]
  (and
   (and (< (min c1x1 c1x2) (max c2x1 c2x2)) (> (max c1x1 c1x2) (min c2x1 c2x2)))
   (and (< (min c1y1 c1y2) (max c2y1 c2y2)) (> (max c1y1 c1y2) (min c2y1 c2y2)))
   (and (< (min c1z1 c1z2) (max c2z1 c2z2)) (> (max c1z1 c1z2) (min c2z1 c2z2)))))

(defn split-cube [c1 c2]
  (let [[c1x1 c1x2 c1y1 c1y2 c1z1 c1z2] c1
        [c2x1 c2x2 c2y1 c2y2 c2z1 c2z2] c2
        xs (apply sorted-set [c1x1 c1x2 c2x1 c2x2])
        ys (apply sorted-set [c1y1 c1y2 c2y1 c2y2])
        zs (apply sorted-set [c1z1 c1z2 c2z1 c2z2])
        xseq (partition 2 1 xs)
        yseq (partition 2 1 ys)
        zseq (partition 2 1 zs)]
    (->> (for [[x1 x2] xseq
               [y1 y2] yseq
               [z1 z2] zseq]
           [x1 x2 y1 y2 z1 z2])
         (filter #(and (intersect? c1 %)
                       (not (intersect? c2 %)))))))

(defn cubic-volume [[x1 x2 y1 y2 z1 z2]]
  (* (- x2 x1) (- y2 y1) (- z2 z1)))

(defn solve [input]
  (reduce
   (fn [cubes [cmd next-cube]]
     (let [new-cubes (vec (mapcat #(if (intersect? % next-cube)
                                     (split-cube % next-cube)
                                     [%]) cubes))]
       (if (= cmd "on")
         (conj new-cubes next-cube)
         new-cubes)))
   []
   input))

(defn part1 [input]
  (let [init-input (map (fn [[cmd [x1 x2 y1 y2 z1 z2]]]
                          [cmd
                           [(max x1 -50) (min x2 50)
                            (max y1 -50) (min y2 50)
                            (max z1 -50) (min z2 50)]]) input)
        cubes (solve init-input)]
    (->> cubes
         (map cubic-volume)
         (reduce +))))

(defn part2 [input]
  (->> (solve input)
       (map cubic-volume)
       (reduce +)))

(defn parse-input [row]
  (let [[cmd coords-raw] (str/split row #" ")
        [x1 x2 y1 y2 z1 z2] (map #(Integer/parseInt %) (re-seq #"-?\d+" coords-raw))]
    [cmd [x1 (inc x2) y1 (inc y2) z1 (inc z2)]]))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (map parse-input in)]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))
