(ns kanban-sim.policies-test
  (:require [clojure.test :refer [deftest is testing]]
            [kanban-sim.model.card :refer [make-card]]
            [kanban-sim.model.members :refer [developers]]
            [kanban-sim.model.policies :refer [assign assign-developer
                                               assign-developer-to-specialty
                                               assign-developer-to-test select-card-with-more-work]]
            [kanban-sim.model.wip-limits :refer [wip-limits]]))


(defn test-cards []
  [(make-card "T1" "test" 10)
   (make-card "T2" "test" 8)
   (make-card "T3" "test" 5)
   (make-card "D1" "development-done" 12)
   (make-card "D2" "development" 8)
   (make-card "D3" "development" 10)
   (make-card "D4" "development" 12)
   (make-card "D5" "development" 8)
   (make-card "A1" "analysis" 6)
   (make-card "A1" "analysis" 7)
              ;
   ])

(deftest assign-test
  (testing "number of cards is same when wip-policy false"
    (let [_ (enable-console-print!)
          cards-to-assign (test-cards)
          assigned-cards (assign {:wip-policy false
                                  :wip-limits wip-limits
                                  :select-card-to-work select-card-with-more-work}
                                 developers
                                 cards-to-assign)]
      (is (= (count cards-to-assign) (count assigned-cards)))))

  (testing "number of cards is same when wip-policy true"
    (let [_ (enable-console-print!)
          cards-to-assign (test-cards)
          assigned-cards (assign {:wip-policy true
                                  :wip-limits wip-limits
                                  :select-card-to-work select-card-with-more-work}
                                 developers
                                 cards-to-assign)]

    ;;   (println (map :Name assigned-cards))
      (is (= (count cards-to-assign) (count assigned-cards)))))

  (testing "cards are same when wip-policy true"
    (let [_ (enable-console-print!)
          assigned-cards (into [] (map #(dissoc % :StoryId)
                                       (assign {:wip-policy true
                                                :wip-limits wip-limits
                                                :select-card-to-work select-card-with-more-work} developers (test-cards))))
          expected-cards [{:Name "D5", :stage "development", :Development 8, :DevelopmentDone 0}
                          {:Name "D4",
                           :stage "development",
                           :Development 12,
                           :DevelopmentDone 0,
                           :developers [{:Active true, :Role "developer", :StoryId nil, :TeamMemberId 6}]}
                          {:Name "D3", :stage "development", :Development 10, :DevelopmentDone 0}
                          {:Name "D2", :stage "development", :Development 8, :DevelopmentDone 0}
                          {:Name "A1",
                           :stage "analysis",
                           :Analysis 6,
                           :AnalysisDone 0,
                           :developers [{:Active true, :Role "analyst", :StoryId nil, :TeamMemberId 2}]}
                          {:Name "A1",
                           :stage "analysis",
                           :Analysis 7,
                           :AnalysisDone 0,
                           :developers [{:Active true, :Role "analyst", :StoryId nil, :TeamMemberId 1}]}
                          {:Name "T1",
                           :stage "test",
                           :Test 10,
                           :TestDone 0,
                           :developers
                           [{:Active true, :Role "tester", :StoryId nil, :TeamMemberId 7}
                            {:Active true, :Role "developer", :StoryId nil, :TeamMemberId 5}]}
                          {:Name "T2",
                           :stage "test",
                           :Test 8,
                           :TestDone 0,
                           :developers [{:Active true, :Role "tester", :StoryId nil, :TeamMemberId 8}]}
                          {:Name "T3",
                           :stage "test",
                           :Test 5,
                           :TestDone 0,
                           :developers [{:Active true, :Role "developer", :StoryId nil, :TeamMemberId 4}]}
                          {:Name "D1", :stage "development-done", :Development 12, :DevelopmentDone 12}]]

      (is (= expected-cards assigned-cards))))

  
)

(into [] (map #(dissoc % :StoryId)
              (assign {:wip-policy false
                       :wip-limits wip-limits
                       :select-card-to-work select-card-with-more-work} developers (test-cards))))


(deftest assign-developer-test
  (testing "number of cards is same"
    (let [_ (enable-console-print!)
          _ (println "TEST")
          cards-to-assign (test-cards)
          assigned-cards (assign-developer {:wip-policy true
                                            :wip-limits wip-limits
                                            :select-card-to-work select-card-with-more-work}
                                           developers
                                           cards-to-assign)]
      (is (= (count cards-to-assign) (count assigned-cards))))))


(deftest assign-developer-to-test-test
  (testing "number of cards is same"
    (let [_ (enable-console-print!)
          _ (println "TEST")
          cards-to-assign (test-cards)
          [devs assigned-cards] (assign-developer-to-test {:wip-policy true
                                                           :wip-limits wip-limits
                                                           :select-card-to-work select-card-with-more-work}
                                                          developers
                                                          cards-to-assign)]
      (is (= (count cards-to-assign) (count assigned-cards)))
      (is (= (count devs) (dec (count developers)))))))

(deftest assign-developer-to-specialty-test
  (testing "number of cards is same"
    (let [_ (enable-console-print!)
          _ (println "TEST")
          cards-to-assign (test-cards)
          [devs assigned-cards] (assign-developer-to-specialty {:wip-policy true
                                                                :wip-limits wip-limits
                                                                :select-card-to-work select-card-with-more-work}
                                                               developers
                                                               cards-to-assign)]
      (is (= (count cards-to-assign) (count assigned-cards)))
      (is (= (count devs) (dec (count developers)))))))


