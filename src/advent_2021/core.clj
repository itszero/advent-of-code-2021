(ns advent-2021.core
  (:gen-class))

(require 'advent-2021.day1)
(require 'advent-2021.day2)
(require 'advent-2021.day3)
(require 'advent-2021.day4)
(require 'advent-2021.day5)
(require 'advent-2021.day6)
(require 'advent-2021.day7)
(require 'advent-2021.day8)
(require 'advent-2021.day9)
(require 'clojure.java.io)

(def -main
  (fn [& args]
    (with-open [file (clojure.java.io/reader (second args))]
      ((case (first args)
         "day1" advent-2021.day1/main
         "day2" advent-2021.day2/main
         "day3" advent-2021.day3/main
         "day4" advent-2021.day4/main
         "day5" advent-2021.day5/main
         "day6" advent-2021.day6/main
         "day7" advent-2021.day7/main
         "day8" advent-2021.day8/main
         "day9" advent-2021.day9/main) file))))
