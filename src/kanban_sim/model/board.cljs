(ns kanban-sim.model.board
  (:require [clojure.pprint :as pprint]
            [kanban-sim.model.card :refer [done? next-stage work-on-card]]
            [kanban-sim.model.cards :refer [all-cards cards->map filter-stage]]
            [kanban-sim.model.members :refer [developers specialty]]))


;;
;; pull using some strategy
;;
(defn remove-card-fun [temp-col from-col]
  (let [[c & cs] from-col]
    (if (done? c)
      [(update c :stage next-stage) (into temp-col cs)]
      (remove-card-fun (into temp-col [c]) cs))))

(defn remove-card [from-col]
  ;; [(first from-col) (into [] (rest from-col))]
  (remove-card-fun [] from-col))

(defn add-card [to-col card]
  (conj to-col card))

(defn pull-card [from-col to-col]
  (let [[card, from-col-upd] (remove-card from-col)]
    [from-col-upd (add-card to-col card)]))


(defn wip [column]
  (count column))


(defn pull-from-to [second-col first-col]
  (if (and (or (nil? (:wip first-col))
               (< (wip (:cards first-col)) (:wip first-col)))
           (>  (count (filter done? (:cards second-col))) 0))
    (let [[from-upd to-upd] (pull-card (:cards second-col) (:cards first-col))]
      (pull-from-to (assoc second-col :cards from-upd) (assoc first-col :cards to-upd)))
    [second-col first-col]))


;; TODO
;; keep pulling when cards can still be pulled
;; e.g. from deck to ready to analysis
(defn pull [columns]
  (let [[first-col second-col & rest-cols] columns]
    (if (nil? second-col)
      [first-col]
      (let [[second-upd first-upd] (pull-from-to second-col first-col)]
        (into [first-upd] (pull (into [second-upd] rest-cols)))))))

(defn create-columns [all-cards]
  [{:label :deployed :cards (filter-stage all-cards "deployed")}
   {:label :test :cards
    (into (filter-stage all-cards "test")
          (filter-stage all-cards "test-done")) :wip 3}
   {:label :development :cards
    (into (filter-stage all-cards "development")
          (filter-stage all-cards "development-done")) :wip 5}
   {:label :analysis :cards
    (into (filter-stage all-cards "analysis")
          (filter-stage all-cards "analysis-done")) :wip 3}
   {:label :ready :cards (filter-stage all-cards "ready") :wip 5}
   {:label :deck :cards
    (filter-stage all-cards "deck")}])

(defn flatten-column-cards [columns]
  (flatten (map :cards columns)))


(defn pull-cards [cards]
  (->> cards
       create-columns
       pull
       flatten-column-cards))

