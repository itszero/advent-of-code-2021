(ns advent-2021.core
  (:gen-class))

(require '[advent-2021.utils :refer [debug]])
(require '[clojure.string :as str])
(require '[clojure.java.io :as jio])

(def days (->> (jio/file "./src/advent_2021")
               (file-seq)
               (filter #(.isFile %))
               (filter #(str/includes? (.getName %) "day"))
               (map #(.getName %))
               (map (fn [file-name] (str/replace file-name #"day(\d+).clj" #(str "day" (%1 1)))))))

(doall (map #(require (symbol (str "advent-2021." %))) days))

(def -main
  (fn [& args]
    (with-open [file (jio/reader (second args))]
      ((ns-resolve (symbol (str "advent-2021." (first args))) (symbol "main")) file))))
