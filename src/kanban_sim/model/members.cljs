(ns kanban-sim.model.members
  (:require clojure.pprint))


(def members-str "[
        {
            \"Active\": true,
            \"Role\": \"analyst\",
            \"StoryId\": null,
            \"TeamMemberId\": 1
        },
        {
            \"Active\": true,
            \"Role\": \"analyst\",
            \"StoryId\": null,
            \"TeamMemberId\": 2
        },
        {
            \"Active\": false,
            \"Role\": \"analyst\",
            \"StoryId\": null,
            \"TeamMemberId\": 3
        },
        {
            \"Active\": true,
            \"Role\": \"developer\",
            \"StoryId\": null,
            \"TeamMemberId\": 4
        },
        {
            \"Active\": true,
            \"Role\": \"developer\",
            \"StoryId\": null,
            \"TeamMemberId\": 5
        },
        {
            \"Active\": true,
            \"Role\": \"developer\",
            \"StoryId\": null,
            \"TeamMemberId\": 6
        },
        {
            \"Active\": true,
            \"Role\": \"tester\",
            \"StoryId\": null,
            \"TeamMemberId\": 7
        },
        {
            \"Active\": true,
            \"Role\": \"tester\",
            \"StoryId\": null,
            \"TeamMemberId\": 8
        },
        {
            \"Active\": false,
            \"Role\": \"tester\",
            \"StoryId\": null,
            \"TeamMemberId\": 9
        }
    ]")



(def member-js (.parse js/JSON members-str))
(def members (js->clj member-js :keywordize-keys true))

(def developers (filter #(:Active %) members))

(defn specialty [developer]
  (cond
    (= (:Role developer) "tester") "test"
    (= (:Role developer) "developer") "development"
    (= (:Role developer) "analyst") "analysis"))

(comment
  (filter #(:Active %) members)
  developers

  (map specialty developers)

  (clojure.pprint/pp)
  ;
  )