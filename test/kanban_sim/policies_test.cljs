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
      (is (= (count cards-to-assign) (count assigned-cards))))))

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