(ns advent-2021.day4)

(require '[clojure.string :as str])

(defn split-by [pred coll & [last-out]]
  (if (empty? coll)
    last-out
    (let [last-out (if (nil? last-out) (vector) last-out)
          split (split-with (complement pred) coll)
          out (first split)
          rest (drop 1 (second split))]
      (split-by pred rest (conj last-out out)))))

(defrecord Input [numbers boards])
(defn parse-input [in]
  (let [bingo-numbers-row (first in)
        bingo-numbers (map #(Integer/parseInt %) (str/split bingo-numbers-row #","))
        board-rows (drop 2 in)
        raw-boards (split-by (partial = "") board-rows)
        board-string-to-int (fn [board]
                              (map (fn [row]
                                     (map #(Integer/parseInt %) (str/split (str/trim row) #"\s+"))) board))
        boards (map board-string-to-int raw-boards)]
    (Input. bingo-numbers boards)))

(defn check-win-boards [boards numbers]
  (let [in-numbers? (fn [num] (some #{num} numbers))
        check-win-row (fn [board]
                        (some (partial every? in-numbers?) board))
        check-win-col (fn [board]
                        (check-win-row (apply (partial map vector) board)))
        check-win-board (fn [board]
                          (or (check-win-row board)
                              (check-win-col board)))]
    (filter check-win-board boards)))

(defn calc-score [win-board drawn-numbers]
  (let [last-drawn (last drawn-numbers)
        sum-all-unmarked (reduce + (->> win-board
                                        flatten
                                        (filter #(not (some #{%} drawn-numbers)))))]
    (* last-drawn sum-all-unmarked)))

(defn part1 [in]
  (let [input (parse-input in)
        reducer (fn [drawn-numbers val]
                  (let [new-drawn-numbers (conj drawn-numbers val)
                        win-boards (check-win-boards (:boards input) new-drawn-numbers)]
                    (if (empty? win-boards)
                      new-drawn-numbers
                      (reduced (list new-drawn-numbers (first win-boards))))))
        result (reduce reducer (vector) (:numbers input))
        drawn-numbers (first result)
        win-board (second result)]
    (calc-score win-board drawn-numbers)))

(defn part2 [in]
  (let [input (parse-input in)
        reducer (fn [state val]
                  (let [drawn-numbers (first state)
                        won-boards (second state)
                        left-boards (nth state 2)
                        new-drawn-numbers (conj drawn-numbers val)
                        win-boards (check-win-boards left-boards new-drawn-numbers)
                        win-boards-configuration (map #(list % new-drawn-numbers) win-boards)
                        next-left-boards (filter #(not (some #{%} win-boards)) left-boards)]
                    (if (empty? win-boards)
                      (list new-drawn-numbers won-boards left-boards)
                      (let [next-state (list new-drawn-numbers (concat won-boards win-boards-configuration) next-left-boards)]
                        (if (= (count next-left-boards) 0)
                          (reduced next-state)
                          next-state)))))
        result (reduce reducer (list (vector) (vector) (:boards input)) (:numbers input))
        win-boards (second result)
        last-won-board-configuration (last win-boards)
        last-won-board (first last-won-board-configuration)
        drawn-numbers (second last-won-board-configuration)]
    (calc-score last-won-board drawn-numbers)))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))]
    (println "part1:" (part1 in))
    (println "part2:" (part2 in))))