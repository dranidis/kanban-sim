(ns kanban-sim.model.card
  (:require clojure.pprint
            [clojure.string :as string]
            [kanban-sim.model.cards :refer [all-cards]]))


(def cardStr "{
            \"Analysis\": 10,
            \"AnalysisDone\": 10,
            \"Blocked\": 0,
            \"BlockedDone\": 0,
            \"BlockedLabel\": null,
            \"DayDeployed\": 0,
            \"DayReady\": 1,
            \"Development\": 10,
            \"DevelopmentDone\": 10,
            \"DueDay\": 0,
            \"Expedited\": false,
            \"IsBlocked\": false,
            \"Label\": null,
            \"Name\": \"S1\",
            \"StoryId\": 101,
            \"StoryOrder\": 1,
            \"Test\": 6,
            \"TestDone\": 0,
            \"Value\": 110,
            \"stage\": \"test\"
        }")


(def card-js (.parse js/JSON cardStr))
(def card (js->clj card-js :keywordize-keys true))



(defn testing? [card]
  (= "test" (:stage card)))

(defn developing? [card]
  (= "development" (:stage card)))

(defn analysing? [card]
  (= "analysis" (:stage card)))

(defn amount-of-work [card member dice]
  (cond
    (and (testing? card)
         (= "tester" (:Role member))) (* 2 dice)
    (and (developing? card)
         (= "developer" (:Role member))) (* 2 dice)
    (and (analysing? card)
         (= "analyst" (:Role member))) (* 2 dice)
    :else dice))

(defn work-done [stage]
  (cond
    (= stage "test") :TestDone
    (= stage "development") :DevelopmentDone
    (= stage "analysis") :AnalysisDone))

(defn done-stage [stage]
  (cond
    ;; (= stage "test") "deployed"
    (= stage "test") "test-done" ;; to keep the symmetry of the board structure
    (= stage "development") "development-done"
    (= stage "analysis") "analysis-done"))

(defn work-to-do [card]
  (cond
    (testing? card) (- (:Test card) (:TestDone card))
    (developing? card) (- (:Development card) (:DevelopmentDone card))
    (analysing? card) (- (:Analysis card) (:AnalysisDone card))))

(defn work-completed? [card]
  (<= (work-to-do card) 0))

;;
;; stage is updated but not the StoryOrder in the new stage
;;


(defn estimate-work-left [card]
  (assoc card :estimated-work-left
         (if (:developers card)
           (let [expected-dice 3.5
                 expected-work (apply + (map #(amount-of-work card % expected-dice) (:developers card)))]
             (- (work-to-do card) expected-work))
           (work-to-do card))))

(defn work-on-card [card]
  (if (:developers card)
    (let [total-work (apply + (map #(amount-of-work card % (inc (rand-int 6))) (:developers card)))
          worked-card (update card (work-done (:stage card)) + total-work)]
      (dissoc
       (if (work-completed? worked-card)
         (update worked-card :stage done-stage)
         worked-card)
       :developers))
    card))

(defn done? [card]
  (and (not (nil? (:stage card)))
       (not (nil? (#{"deck" "ready" "analysis-done" "development-done" "test-done" "deployed"} (:stage card))))))

(defn next-stage [stage]
  (cond
    (= stage "test-done") "deployed"
    (= stage "development-done") "test"
    (= stage "analysis-done") "development"
    (= stage "ready") "analysis"
    (= stage "deck") "ready"))

(defn cycle-time [card]
  (- (:DayDeployed card) (:DayReady card)))

(comment
  card

  (if (#{"deck" "ready" "analysis-done" "development-done" "test-done"} "reay") true false)

  (def worked (work-on-card card))
  (:stage worked)

  (string/includes? (:stage worked) "done")
  (done? 1)
  (done? {:stage "deployed"})

  (map :stage all-cards)
  (map estimate-work-left all-cards)

  (take 3 (sort
           (fn [c1 c2] (> (:estimated-work-left c1)
                          (:estimated-work-left c2)))
           (map estimate-work-left all-cards)))
 ;
  )