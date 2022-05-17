(ns kanban-sim.model.policies
  (:require [kanban-sim.model.card :refer [done-stage estimate-work-left]]
            [kanban-sim.model.cards :refer [all-cards]]
            [kanban-sim.model.members :refer [developers specialty]]))

(defn specialty-cards [developer cards]
  (filter #(= (:stage %) (specialty developer)) cards))

(defn done-specialty-cards [developer cards]
  (filter #(= (:stage %) (done-stage (specialty developer))) cards))

(defn non-specialty-cards [developer cards]
  (filter #(not= (:stage %) (specialty developer)) cards))


(defn select-cards-according-to-specialty [developer cards]
  (let [specialty-cards (specialty-cards developer cards)]
    (if (> (count specialty-cards) 0)
      specialty-cards
      (non-specialty-cards developer cards))))

;;
;; when there is at least one card in done phase select another column
;;
(defn select-cards-according-to-specialty-and-done [developer cards]
  (let [specialty-cards (specialty-cards developer cards)]
    (if (and (> (count specialty-cards) 0)
             (= (count (done-specialty-cards developer cards)) 0)
             (> (count
                 (filter #(> (:estimated-work-left %) 0)
                         (map estimate-work-left specialty-cards))) 0))
      specialty-cards
      (non-specialty-cards developer cards))))

;;
;; assign each developer to a random card 
;; first looks in their specialty
;;
(defn select-random-card-to-work [developer cards]
  (rand-nth (select-cards-according-to-specialty developer cards)))

;;
;; assign each developer to the card with the most estimated work left
;; first in their specialty
;;
(defn select-card-with-more-work [developer cards]
  (first (sort
          (fn [c1 c2] (> (:estimated-work-left c1)
                         (:estimated-work-left c2)))
          (map estimate-work-left (select-cards-according-to-specialty developer cards)))))

(defn select-card-with-more-work-considering-done [developer cards]
  (first (sort
          (fn [c1 c2] (> (:estimated-work-left c1)
                         (:estimated-work-left c2)))
          (map estimate-work-left (select-cards-according-to-specialty-and-done developer cards)))))

;;
;; assign each developer to the card with the LEAST positive estimated work left
;; first in their specialty
;;
(defn select-card-with-least-work [developer cards]
  (first (sort
          (fn [c1 c2] (< (:estimated-work-left c1)
                         (:estimated-work-left c2)))
          (filter #(> (:estimated-work-left %) 0)
                  (map estimate-work-left (select-cards-according-to-specialty developer cards))))))

(defn select-card-with-least-work-mixed-considering-done [developer cards]
  (first (sort
          (fn [c1 c2] (< (:estimated-work-left c1)
                         (:estimated-work-left c2)))
          (filter #(> (:estimated-work-left %) 0)
                  (map estimate-work-left (select-cards-according-to-specialty-and-done developer cards))))))

(comment
  (select-cards-according-to-specialty-and-done (first developers) all-cards)

  (done-specialty-cards (nth developers 6) all-cards)
   ;
  )