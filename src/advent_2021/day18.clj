(ns advent-2021.day18)

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.edn :as edn])
(require '[advent-2021.utils :refer [pick debug]])

(defn snailfish-seq
  ([input] (snailfish-seq input '[]))
  ([input path] (if (number? (get-in input path))
                  (vector (list path (get-in input path)))
                  (lazy-seq (concat (snailfish-seq input (conj path 0))
                                    (snailfish-seq input (conj path 1)))))))

(defn snailfish-explode [input]
  (let [num-seq (lazy-seq (concat '((nil nil)) (snailfish-seq input) '((nil nil) (nil nil))))
        partitioned-seq (partition 4 1 num-seq)
        filtered-seq (drop-while #(< (count (first (second %))) 5) partitioned-seq)
        to-split (first filtered-seq)]
    (if (some? to-split)
      (let [[[left-path left-num]
             [mid-left-path mid-left-num]
             [_ mid-right-num]
             [right-path right-num]] to-split
            pair-path (drop-last mid-left-path)]
        (list :changed (-> input
                           ((fn [input] (if (some? left-path) (assoc-in input left-path (+ left-num mid-left-num)) input)))
                           ((fn [input] (if (some? right-path) (assoc-in input right-path (+ right-num mid-right-num)) input)))
                           (assoc-in pair-path 0))))
      (list :same input))))

(defn snailfish-split [input]
  (let [num-seq (snailfish-seq input)
        filtered-seq (drop-while #(< (second %) 10) num-seq)
        to-split (first filtered-seq)]
    (if (some? to-split)
      (let [half (float (/ (second to-split) 2))
            lower (int (Math/floor half))
            upper (int (Math/ceil half))]
        (list :changed (assoc-in input (first to-split) [lower upper])))
      (list :same input))))

(defn snailfish-reduce [input]
    (let [[next-status input] (snailfish-explode input)]
      (if (= next-status :changed)
        (snailfish-reduce input)
        (let [[next-status input] (snailfish-split input)]
          (if (= next-status :changed)
            (snailfish-reduce input)
            input)))))

(defn snailfish-add [nums]
  (if (= (count nums) 1)
    (first nums)
    (let [added-num [(first nums), (second nums)]
          reduced-num (snailfish-reduce added-num)]
      (snailfish-add (cons reduced-num (drop 2 nums))))))

(defn snailfish-magnitude [number]
  (let [left-num (first number)
        right-num (second number)
        left (if (number? left-num) left-num (snailfish-magnitude left-num))
        right (if (number? right-num) right-num (snailfish-magnitude right-num))]
    (+ (* 3 left) (* 2 right))))

(defn part1 [in]
  (snailfish-magnitude (snailfish-add in)))

(defn part2 [in]
  (->> in
       (vec)
       (pick 2)
       (mapcat #(vec [(snailfish-add [(first %) (second %)])
                  (snailfish-add [(second %) (first %)])]))
       (map snailfish-magnitude)
       (apply max)))

(deftest test-1
  (is (= (snailfish-add [[1,1]
                         [2,2]
                         [3,3]
                         [4,4]])
         [[[[1,1],[2,2]],[3,3]],[4,4]])))

(deftest test-2
  (is (= (snailfish-add [[1,1]
                         [2,2]
                         [3,3]
                         [4,4]
                         [5,5]])
         [[[[3,0],[5,3]],[4,4]],[5,5]])))

(deftest test-3
  (is (= (snailfish-add [[1,1]
                         [2,2]
                         [3,3]
                         [4,4]
                         [5,5]
                         [6,6]])
         [[[[5,0],[7,4]],[5,5]],[6,6]])))

(deftest test-4
  (is (= (snailfish-add [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                         [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
                         [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
                         [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
                         [7,[5,[[3,8],[1,4]]]]
                         [[2,[2,2]],[8,[8,1]]]
                         [2,9]
                         [1,[[[9,3],9],[[9,0],[0,7]]]]
                         [[[5,[7,4]],7],1]
                         [[[[4,2],2],6],[8,7]]])
         [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])))

(deftest test-5
  (is (= (snailfish-magnitude [[1,2],[[3,4],5]]) 143)))

(deftest test-6
  (is (= (snailfish-magnitude [[[[0,7],4],[[7,8],[6,0]]],[8,1]]) 1384)))

(deftest test-7
  (is (= (snailfish-magnitude [[[[1,1],[2,2]],[3,3]],[4,4]]) 445)))

(deftest test-8
  (is (= (snailfish-magnitude [[[[3,0],[5,3]],[4,4]],[5,5]]) 791)))

(deftest test-9
  (is (= (snailfish-magnitude [[[[5,0],[7,4]],[5,5]],[6,6]]) 1137)))

(deftest test-10
  (is (= (snailfish-magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]) 3488)))

(deftest test-11
  (is (= (snailfish-add [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                         [[[5,[2,8]],4],[5,[[9,9],0]]]
                         [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                         [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                         [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                         [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                         [[[[5,4],[7,7]],8],[[8,3],8]]
                         [[9,3],[[9,9],[6,[4,9]]]]
                         [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                         [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])
         [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]])))

(deftest test-12
  (is (= (snailfish-magnitude [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]) 4140)))

(defn main [file]
  (run-tests 'advent-2021.day18)
  (let [in (line-seq (java.io.BufferedReader. file))
        input (map edn/read-string in)]
    (println "part1:" (part1 input))
    (println "part2:" (part2 input))))
