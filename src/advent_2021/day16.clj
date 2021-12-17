(ns advent-2021.day16)

(require '[clojure.string :as str])

(defn hex-to-binary [in]
  (apply str (map #(->> (str
                         "0000"
                         (Integer/toString (Integer/parseInt (str %) 16) 2))
                        (take-last 4)
                        (str/join)) in)))

(defrecord Packet [version type content])

(declare parse-packet)

(defn parse-literal-packet [rest-bin]
  (loop [rest-bin rest-bin
         val-bin ""]
    (let [group-type (Integer/parseInt (str/join (take 1 rest-bin)) 2)
          group-value (str/join (take 4 (drop 1 rest-bin)))
          rest-bin (str/join (drop 5 rest-bin))]
      (case group-type
        1 (recur rest-bin (str val-bin group-value))
        0 (list (BigInteger. (str val-bin group-value) 2) rest-bin)))))

(defn parse-operator-packet [rest-bin]
  (let [length-type (Integer/parseInt (str/join (take 1 rest-bin)) 2)
        rest-bin (str/join (drop 1 rest-bin))]
    (case length-type
      0 (let [length (Integer/parseInt (str/join (take 15 rest-bin)) 2)
              rest-bin (str/join (drop 15 rest-bin))
              extra-bin (str/join (drop length rest-bin))]
          (loop [packets '[]
                 rest-bin (str/join (take length rest-bin))]
            (if (> (count rest-bin) 6)
              (let [[packet rest-bin] (parse-packet rest-bin true)]
                (recur (conj packets packet) rest-bin))
              (list packets extra-bin))))
      1 (let [length (Integer/parseInt (str/join (take 11 rest-bin)) 2)
              rest-bin (str/join (drop 11 rest-bin))]
          (loop [packets '[]
                 rest-bin rest-bin]
            (if (< (count packets) length)
              (let [[packet rest-bin] (parse-packet rest-bin true)]
                (recur (conj packets packet) rest-bin))
              (list packets rest-bin)))))))

(defn parse-packet [in & is-binary]
  (let [bin-in (if is-binary in (hex-to-binary in))
        version (Integer/parseInt (str/join (take 3 bin-in)) 2)
        type (Integer/parseInt (str/join (take 3 (drop 3 bin-in))) 2)
        rest-bin (str/join (drop 6 bin-in))]
    (case type
      4 (let [[content rest-bin] (parse-literal-packet rest-bin)]
          (list (Packet. version type content) rest-bin))
      (let [[content rest-bin] (parse-operator-packet rest-bin)]
        (list (Packet. version type content) rest-bin)))))

(defn traverse-packet [packet]
  (apply (partial concat (list packet))
         (if (= (:type packet) 4)
           '(())
           (map #(traverse-packet %) (:content packet)))))

(defn part1 [packet]
  (->> packet
       (traverse-packet)
       (map :version)
       (reduce +)))

(defn execute-packet [packet]
  (case (:type packet)
    0 (reduce + (map execute-packet (:content packet)))
    1 (reduce * (map execute-packet (:content packet)))
    2 (apply min (map execute-packet (:content packet)))
    3 (apply max (map execute-packet (:content packet)))
    4 (:content packet)
    5 (if (> (execute-packet (first (:content packet))) (execute-packet (second (:content packet)))) 1 0)
    6 (if (< (execute-packet (first (:content packet))) (execute-packet (second (:content packet)))) 1 0)
    7 (if (= (execute-packet (first (:content packet))) (execute-packet (second (:content packet)))) 1 0)))

(defn part2 [packet]
  (execute-packet packet))

(defn main [file]
  (let [in (line-seq (java.io.BufferedReader. file))]
    (map #(let [[packet _] (parse-packet %)]
            (println "part1:" (part1 packet))
            (println "part2:" (part2 packet))) in)))
