(ns advent-2021.day5)

(require '[clojure.string :as str])
(require '[advent-2021.utils :refer [map-map-values]])

(defrecord Line [x1 y1 x2 y2])

(defn parse-pair [pair]
  (let [nums (str/split pair #",")
        x (Integer/parseInt (first nums))
        y (Integer/parseInt (second nums))]
    (list x y)))
(defn parse-input-lines [in]
  (let [parse-input-line (fn [line]
                           (let [split-line (str/split line #" -> ")
                                 first-pair (parse-pair (first split-line))
                                 second-pair (parse-pair (second split-line))]
                             (Line. (first first-pair) (second first-pair)
                                    (first second-pair) (second second-pair))))]
    (map parse-input-line in)))

(defn generate-range [a b]
  (let [min (min a b)
        max (max a b)
        out-range (range min (+ max 1))]
    (if (< a b) out-range (reverse out-range))))

(defn is-straight-line [line]
  (or (= (:x1 line) (:x2 line)) (= (:y1 line) (:y2 line))))

(defn generate-straight-points [line]
  (let [x1 (:x1 line)
        x2 (:x2 line)
        y1 (:y1 line)
        y2 (:y2 line)
        rangex (generate-range x1 x2)
        rangey (generate-range y1 y2)]
    (mapcat (fn [x] (map (fn [y] (list x y)) rangey)) rangex)))

(defn generate-diagonal-points [line]
  (let [x1 (:x1 line)
        x2 (:x2 line)
        y1 (:y1 line)
        y2 (:y2 line)
        rangex (generate-range x1 x2)
        rangey (generate-range y1 y2)]
    (map list rangex rangey)))

(defn generate-points [line]
  (if (is-straight-line line)
    (generate-straight-points line)
    (generate-diagonal-points line)))

(defn part1 [in]
  (let [lines (filter is-straight-line (parse-input-lines in))
        all-points (mapcat generate-points lines)
        groups (group-by identity all-points)
        all-counts (map-map-values count groups)
        more-than-2-cross (filter (fn [key] (>= (get all-counts key 0) 2)) (keys all-counts))]
    (count more-than-2-cross)))

(defn part2 [in]
  (let [lines (parse-input-lines in)
        all-points (mapcat generate-points lines)
        groups (group-by identity all-points)
        all-counts (map-map-values count groups)
        more-than-2-cross (filter (fn [key] (>= (get all-counts key 0) 2)) (keys all-counts))]
    (count more-than-2-cross)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))]
    (println "part1:" (part1 in))
    (println "part2:" (part2 in))))