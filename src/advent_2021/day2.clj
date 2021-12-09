(ns advent-2021.day2)

(require '[clojure.string :as str])

(def parse-line
  (fn [line]
    (let [parts (str/split line #" ")]
      (list (first parts) (Integer/parseInt (second parts))))))

(def part1
  (fn [in] (let [instructions (map parse-line in)
                 initial-state '(0 0)
                 proc (fn [state instruction]
                        (let [command (first instruction)
                              steps (second instruction)
                              horizontal (first state)
                              depth (second state)]
                          (case command
                            "forward" (list (+ horizontal steps) depth)
                            "up" (list horizontal (- depth steps))
                            "down" (list horizontal (+ depth steps)))))
                 out (reduce proc initial-state instructions)]
             (reduce * out))))

(def part2
  (fn [in] (let [instructions (map parse-line in)
                 initial-state '(0 0 0)
                 proc (fn [state instruction]
                        (let [command (first instruction)
                              steps (second instruction)
                              horizontal (first state)
                              depth (second state)
                              aim (nth state 2)]
                          (case command
                            "forward" (list (+ horizontal steps) (+ depth (* aim steps)) aim)
                            "up" (list horizontal depth (- aim steps))
                            "down" (list horizontal depth (+ aim steps)))))
                 out (reduce proc initial-state instructions)]
             (reduce * (drop-last out)))))

(def main
  (fn [file] (let [in (line-seq (java.io.BufferedReader. file))]
               (println "part1:" (part1 in))
               (println "part2:" (part2 in)))))