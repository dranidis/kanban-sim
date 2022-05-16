(ns kanban-sim.model.policies 
  (:require [kanban-sim.model.card :refer [estimate-work-left]]
            [kanban-sim.model.members :refer [specialty]]))

(defn specialty-cards [developer cards]
  (filter #(= (:stage %) (specialty developer)) cards))

(defn non-specialty-cards [developer cards]
  (filter #(not= (:stage %) (specialty developer)) cards))

;;
;; assign each developer to a random card 
;; first looks in their specialty
;;
(defn find-random-card-to-work [developer cards]
  (let [specialty-cards (specialty-cards developer cards)]
    (rand-nth (if (> (count specialty-cards) 0)
                specialty-cards
                (non-specialty-cards developer cards)))))

;;
;; assign each developer to the card with the most estimated work left
;; first in their specialty
;;
(defn find-card-with-more-work [developer cards]
  (let [specialty-cards (specialty-cards developer cards)]
    (first (sort
            (fn [c1 c2] (> (:estimated-work-left c1)
                           (:estimated-work-left c2)))
            (map estimate-work-left (if (> (count specialty-cards) 0)
                                      specialty-cards
                                      (non-specialty-cards developer cards)))))))