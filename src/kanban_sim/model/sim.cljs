(ns kanban-sim.model.sim 
  (:require [cljs.pprint :as pprint]
            [kanban-sim.model.board :refer [create-columns pull-cards]]
            [kanban-sim.model.card :refer [done? work-on-card]]
            [kanban-sim.model.cards :refer [all-cards cards->map]]
            [kanban-sim.model.members :refer [developers specialty]]))

;;
;; assign each developer to a random card in their specialty
;;

(defn assign-to-card [card developer]
  (let [developers (:developers card)]
    (if developers
      (update card :developers conj developer)
      (assoc card :developers [developer]))))

(defn assign-developer [developer cards]
  (let [cards-map (cards->map cards)
        specialty-cards (filter #(= (:stage %) (specialty developer)) cards)]
    (if (> (count specialty-cards) 0)
      (let [random-story-id (:StoryId (rand-nth specialty-cards))
            assigned-card (assign-to-card (cards-map random-story-id) developer)]
        (into [] (vals (assoc cards-map random-story-id assigned-card))))
      cards)))

(defn assign [developers cards]
  (let [[developer & devs] developers]
    (if developer
      (assign devs (assign-developer developer cards))
      cards)))

(defn update-card-day-ready [day card]
  (if (and (= (:stage card) "ready")
           (= (:DayReady card) 0))
    (assoc card :DayReady day)
    card))

(defn update-card-day-deployed [day card]
  (if (and (= (:stage card) "deployed")
           (= (:DayDeployed card) 0))
    (assoc card :DayDeployed day)
    card))

(defn update-day-deployed [day cards]
  (map #(update-card-day-ready day (update-card-day-deployed day %)) cards))

(defn develop-cycle [day developers cards]
  (->> cards
       (assign developers)
       (map work-on-card)
       pull-cards
       (update-day-deployed day)))

(defn log-cards [cards]
  (print "LOG")
  (pprint/pprint (->> cards
                      create-columns
                      (filter #(not= (:label %) :deck))
                      (map #(map
                             (fn [c] [(:Name c) (:stage c) (:DayReady c) (:DayDeployed c) (done? c) (:developers c)])
                             (:cards %)))))
  (println)
  cards)

(def start-day 9)

(defn day [day-nr developers cards]
  (let [unfinished-cards (count (filter #(not (done? %)) cards))]
    (when (and (< day-nr 23) (> unfinished-cards 0))
      (println "AT BEGINNING OF DAY" day-nr)
      (log-cards cards)
      (day (inc day-nr) developers (develop-cycle day-nr developers cards)))))

(comment
  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       all-cards)

  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign-developer (nth developers 0) all-cards))

  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign developers all-cards))


;; -----------------

  (day start-day developers all-cards)

  (->> all-cards
       log-cards

       (develop-cycle 11 developers)
       log-cards

       (develop-cycle 12 developers)
       log-cards

       (develop-cycle 13 developers)
       log-cards

       create-columns
       (filter #(not= (:label %) :deck))
       (map #(map
              (fn [c] [(:StoryId c) (:stage c) (done? c) (:developers c)])
              (:cards %))))


  (pprint/pp)
  ;
  )
