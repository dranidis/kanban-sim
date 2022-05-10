(ns kanban-sim.model.card 
  (:require [clojure.string :as string]))

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


(def members-str "[
        {
            \"Active\": false,
            \"Role\": \"analyst\",
            \"StoryId\": null,
            \"TeamMemberId\": 1
        },
        {
            \"Active\": true,
            \"Role\": \"analyst\",
            \"StoryId\": 103,
            \"TeamMemberId\": 2
        },
        {
            \"Active\": true,
            \"Role\": \"analyst\",
            \"StoryId\": 103,
            \"TeamMemberId\": 3
        },
        {
            \"Active\": false,
            \"Role\": \"developer\",
            \"StoryId\": null,
            \"TeamMemberId\": 4
        },
        {
            \"Active\": true,
            \"Role\": \"developer\",
            \"StoryId\": 102,
            \"TeamMemberId\": 5
        },
        {
            \"Active\": true,
            \"Role\": \"developer\",
            \"StoryId\": 102,
            \"TeamMemberId\": 6
        },
        {
            \"Active\": true,
            \"Role\": \"tester\",
            \"StoryId\": 106,
            \"TeamMemberId\": 7
        },
        {
            \"Active\": true,
            \"Role\": \"tester\",
            \"StoryId\": null,
            \"TeamMemberId\": 8
        },
        {
            \"Active\": true,
            \"Role\": \"tester\",
            \"StoryId\": null,
            \"TeamMemberId\": 9
        }
    ]")

(def card-js (.parse js/JSON cardStr))
(def card (js->clj card-js :keywordize-keys true))

(def member-js (.parse js/JSON members-str))
(def members (js->clj member-js :keywordize-keys true))

(def workers [(members 1) (members 5) (members 8)])




(defn testing? [card]
  (= "test" (:stage card)))

(defn developing? [card]
  (= "development" (:stage card)))

(defn analysing? [card]
  (= "analysis" (:stage card)))

(defn roll-dice [card member]
  (let [dice (inc (rand-int 6))]
    (cond
      (and (testing? card)
           (= "tester" (:Role member))) (* 2 dice)
      (and (developing? card)
           (= "developer" (:Role member))) (* 2 dice)
      (and (analysing? card)
           (= "analyst" (:Role member))) (* 2 dice)
      :else dice)))

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

(defn work-completed? [card]
  (cond
    (testing? card) (>= (:TestDone card) (:Test card))
    (developing? card) (>= (:DevelopmentDone card) (:Development card))
    (analysing? card) (>= (:AnalysisDone card) (:Analysis card))))

;;
;; stage is updated but not the StoryOrder in the new stage
;;
(defn work [card members]
  (let [total (apply + (map #(roll-dice card %) members))
        worked-card (update card (work-done (:stage card)) + total)]
    (if (work-completed? worked-card)
      (update worked-card :stage done-stage)
      worked-card)))


(defn done? [card]
  (string/includes? (:stage card) "done"))

(
 comment
 card

 (def worked (work card workers))
 (:stage worked)

 (string/includes? (:stage worked) "done")
 ;
)