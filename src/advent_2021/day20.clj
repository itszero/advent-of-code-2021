(ns advent-2021.day20)

(require '[clojure.string :as str])
(require '[advent-2021.utils :refer [pick debug]])

(defrecord Input [table image])
(defn parse-input [in]
  (let [table (str/split (first in) #"")
        image-arr (mapv #(str/split % #"") (drop 2 in))
        image-seq (for [y (range 0 (count image-arr))
                        x (range 0 (count (first image-arr)))]
                    [(list y x) (get-in image-arr (list y x))])
        image-map (apply hash-map (mapcat identity image-seq))]
    (Input. table image-map)))

(defn get-pixels [image default y x]
  (let [directions [[-1 -1]
                    [-1 0]
                    [-1 1]
                    [0 -1]
                    [0 0]
                    [0 1]
                    [1 -1]
                    [1 0]
                    [1 1]]
        coordinates (mapv #(vec [(+ (first %) y) (+ (second %) x)]) directions)
        pixels (mapv #(get image % default) coordinates)
        binary (mapv #(case %
                 "#" "1"
                 "." "0") pixels)]
    (Integer/parseInt (str/join binary) 2)))

(defn enhance-pixel [table image default y x]
  (get table (get-pixels image default y x)))

(defn enhance [round table image]
  (let [y-s (map first (keys image))
        x-s (map second (keys image))
        min-y (apply min y-s)
        max-y (apply max y-s)
        min-x (apply min x-s)
        max-x (apply max x-s)
        coordinates (for [y (range (- min-y 3) (+ max-y 4))
                          x (range (- min-x 3) (+ max-x 4))]
                      [y x])
        new-image (map #(vec [% (apply (partial enhance-pixel table image (if (= 0 (rem round 2)) "." "#")) %)]) coordinates)]
    (apply hash-map (mapcat identity new-image))))

(defn print-map [round image]
  (let [y-s (map first (keys image))
        x-s (map second (keys image))
        min-y (apply min y-s)
        max-y (apply max y-s)
        min-x (apply min x-s)
        max-x (apply max x-s)]
    (doseq [y (range (- min-y 2) (+ max-y 3))]
      (doseq [x (range (- min-x 2) (+ max-x 3))]
        (print (get image [y x] (if (= 0 (rem round 2)) "." "#"))))
      (println ""))
    image))

(defn should-flip [table round]
  (if (= (get table 0) "#")
    round
    0))

(defn part1 [input]
  (->> (:image input)
       (enhance (should-flip (:table input) 0) (:table input))
       (enhance (should-flip (:table input) 1) (:table input))
       (vals)
       (map #(case %
               "#" 1
               "." 0))
       (reduce +)))

(defn part2 [input]
  (->> (reduce #(enhance (should-flip (:table input) %2) (:table input) %1) (:image input) (range 0 50))
       (vals)
       (map #(case %
               "#" 1
               "." 0))
       (reduce +)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (parse-input in)]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))
