(ns kanban-sim.model.policies
  (:require [kanban-sim.model.board :refer [est-analysis-done
                                            est-development-done]]
            [kanban-sim.model.card :refer [done-stage estimate-work-left]]
            [kanban-sim.model.cards :refer [all-cards cards->map]]
            [kanban-sim.model.members :refer [developers get-tester get-for-development specialty]]
            [kanban-sim.model.wip-limits :refer [development-wip-limit
                                                 test-wip-limit]]))

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


(defn assign-to-card [card developer]
  (let [developers (:developers card)]
    (if developers
      (update card :developers conj developer)
      (assoc card :developers [developer]))))

(defn assign-developer [policy developer cards]
  (let [cards-map (cards->map cards)
        ;; story-id (:StoryId (find-random-card-to-work developer cards))
        ;; story-id (:StoryId (find-card-with-more-work developer cards))
        story-id (:StoryId ((:select-card-to-work policy) developer cards))
        assigned-card (assign-to-card (cards-map story-id) developer)]
    (into [] (vals (assoc cards-map story-id assigned-card)))))


(defn assign-developer-to-test [policy developers cards]
  ;; (println "To test")
  (let [developer (get-tester developers)
        others (filter #(not= developer %) developers)
        test-cards (filter #(= (:stage %) "test") cards)
        rest-cards (filter #(not= (:stage %) "test") cards)]
    [others (into rest-cards (assign-developer policy developer test-cards))]))

(defn- assign-developer-to-development [policy developers cards]
  ;; (println "To dev")
  (let [developer (get-for-development developers)
        others (filter #(not= developer %) developers)
        test-cards (filter #(= (:stage %) "development") cards)
        rest-cards (filter #(not= (:stage %) "development") cards)]
    [others (into rest-cards (assign-developer policy developer test-cards))]))

(defn assign-developer-to-specialty [policy developers cards]
  ;; (println "To specialty")
  ;; (println "ASSIGN: " (map :Role developers))
  ;; (println "CARDS: " (map :Name cards))
  (let [developer (first developers)
        others (rest developers)
        specialty-cards (filter #(= (:stage %) (specialty developer)) cards)
        rest-cards (filter #(not= (:stage %) (specialty developer)) cards)
        ;; _ (println (specialty developer)
        ;;            (map :Name specialty-cards) (map :Name rest-cards))
        ]
    [others (if (> (count specialty-cards) 0)
              (into rest-cards (assign-developer policy developer specialty-cards))
              (into [] (assign-developer policy developer rest-cards)))]))

(defn assign [policy developers cards]
  ;; (println "ASSIGN: " (map :Role developers))
  ;; (println "CARDS: " (map :Name cards))
  (if (> (count developers) 0)
    (if (:wip-policy policy)
      (if (> (est-development-done cards) (test-wip-limit (:wip-limits policy)))
        (let [[developers' cards'] (assign-developer-to-test policy developers cards)]
          (assign policy developers' cards'))
        (if (> (est-analysis-done cards) (development-wip-limit (:wip-limits policy)))
          (let [[developers' cards'] (assign-developer-to-development policy developers cards)]
            (assign policy developers' cards'))
          (let [[developers' cards'] (assign-developer-to-specialty policy developers cards)]
            (assign policy developers' cards'))))
      (let [[developer & devs] developers]
        (if developer
          (assign policy devs (assign-developer policy developer cards))
          cards)))
    cards))



(comment
  (select-cards-according-to-specialty-and-done (first developers) all-cards)

  (done-specialty-cards (nth developers 6) all-cards)
   ;
  )