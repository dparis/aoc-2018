(ns aoc-2018.day-7-test
  (:require [aoc-2018.day-7 :as sut
             :refer [parse-input
                     init-graph
                     step-order
                     all-minimal-nodes
                     build-all-until-first-lex
                     build-parallel-with-lex-cost]]
            [clojure.test :refer :all]
            [clojure.string :as str]
            [loom.graph :as g]))


(def day-7-1-steps
  "Step C must be finished before step A can begin.
   Step C must be finished before step F can begin.
   Step A must be finished before step B can begin.
   Step A must be finished before step D can begin.
   Step B must be finished before step E can begin.
   Step D must be finished before step E can begin.
   Step F must be finished before step E can begin.")

(deftest day-7-1
  (testing "first lexicographical step order"
    (let [graph                 (init-graph (parse-input day-7-1-steps))
          cycle-graph           (g/add-edges graph ["E" "C"])
          multi-ind-start-graph (g/add-edges graph ["G" "F"])
          build-work-fn         build-all-until-first-lex]
      (is (= "CABDFE" (step-order graph all-minimal-nodes (build-work-fn))))
      (is (nil? (step-order cycle-graph all-minimal-nodes (build-work-fn))))
      (is (= "CGABDFE" (step-order multi-ind-start-graph
                                   all-minimal-nodes
                                   (build-work-fn))))))

  (testing "parallel worker step order with lexicographical scheduling"
    (let [graph   (init-graph (parse-input day-7-1-steps))
          time_   (atom -1)
          work-fn (build-parallel-with-lex-cost 2 time_ identity)]
      (is (= "CABFDE" (step-order graph all-minimal-nodes work-fn)))
      (is (= 15 @time_)))))
