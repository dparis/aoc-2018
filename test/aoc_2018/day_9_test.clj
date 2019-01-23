(ns aoc-2018.day-9-test
  (:require [aoc-2018.day-9 :as sut
             :refer [game-fast highest-after-turns]]
            [clojure.test :refer :all]))


(def day-9-1-game-outcomes
  [{:players 10
    :turns   1618
    :score   8317}
   {:players 13
    :turns   7999
    :score   146373}
   {:players 17
    :turns   1104
    :score   2764}
   {:players 21
    :turns   6111
    :score   54718}
   {:players 30
    :turns   5807
    :score   37305}])

(deftest day-9-1
  (testing "highest score after turns"
    (doseq [game-outcome day-9-1-game-outcomes
            :let [players (:players game-outcome)
                  turns   (:turns game-outcome)
                  score   (:score game-outcome)]]
      (is (= score (-> (game-fast players) (highest-after-turns turns) (last)))))))
