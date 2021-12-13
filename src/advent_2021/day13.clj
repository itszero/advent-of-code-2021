(ns advent-2021.day13)

(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[advent-2021.utils :refer [map-map-values]])

(defrecord Input [paper instructions])
(defn parse-input [in]
  (let [coords (->>  in
                     (take-while (complement #{""}))
                     (map (fn [pair] (map #(Integer/parseInt %) (str/split pair #",")))))
        raw-instructions (drop (+ 1 (count coords)) in)
        paper (reduce (fn [paper coord]
                        (assoc paper (second coord)
                               (conj (get paper (second coord) (set '()))
                                     (first coord))))
                      '{}
                      coords)
        instructions (map #(let [[_ axis at] (re-matches #"fold along ([xy])=(\d+)" %)]
                             (list axis (Integer/parseInt at))) raw-instructions)]
    (Input. paper instructions)))

(defn fold-along [paper axis at]
  (case axis
    "x" (map-map-values (fn [row]
                          (let [cols-to-mirror (filter #(> % at) row)]
                            (->> row
                                 (set/select #(< % at))
                                 (set/union (set (map #(- (* 2 at) %) cols-to-mirror))))))
                        paper)
    "y" (let [rows-to-mirror (filter #(> % at) (keys paper))]
          (->> paper
               ((fn [paper] (reduce #(assoc %1 (- (* 2 at) %2) (set/union (get %1 (- (* 2 at) %2) (set '())) (get %1 %2))) paper rows-to-mirror)))
               ((fn [paper] (reduce #(dissoc %1 %2) paper rows-to-mirror)))))))

(defn part1 [input]
  (let [first-fold-paper (apply (partial fold-along (:paper input)) (first (:instructions input)))]
    (reduce + (map count (vals first-fold-paper)))))

(defn print-paper [paper]
  (let [max-y (apply max (keys paper))
        max-x (apply max (map #(apply max %) (vals paper)))]
    (doall (for [y (range 0 (inc max-y))]
             (do
               (doall (for [x (range 0 (inc max-x))]
                        (print (if (contains? (get paper y (set '())) x) "#" "."))))
               (println ""))))
    nil))

(defn part2 [input]
  (let [folded-paper (reduce #(apply (partial fold-along %1) %2) (:paper input) (:instructions input))]
    (print-paper folded-paper)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))
        input (parse-input in)]
    (println "part1:" (part1 input))
    (part2 input)))