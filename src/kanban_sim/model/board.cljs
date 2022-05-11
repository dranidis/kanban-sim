(ns kanban-sim.model.board
  (:require [clojure.pprint :as pprint]
            [kanban-sim.model.card :refer [done? next-stage]]
            [kanban-sim.model.cards :refer [all-cards filter-stage]]
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

;;
;; assign each developer to a random card in their specialty
;;

(defn assign-to-card [cards developer]
  )

(defn assign-developer [temp-cols columns developer]
  (let [[column & cols] columns]
    (if (= (:label column) (specialty developer))
           (assoc column :cards (assign-to-card (:cards column) developer))
           ())))

(defn assign [columns developers]
  (let [[developer & devs] developers]
    (if developer
      (assign (assign-developer [] columns developer) devs)
      columns)))

(comment
  ;; or done!!!!
  (def columns [{:label :deployed :cards (filter-stage all-cards "deployed")}
                {:label :test :cards (filter-stage all-cards "test") :wip 2}
                {:label :development :cards 
                 (into (filter-stage all-cards "development")
                       (filter-stage all-cards "development-done")) :wip 5}
                {:label :analysis :cards 
                (into (filter-stage all-cards "analysis")
                      (filter-stage all-cards "analysis-done")) :wip 4}
                {:label :ready :cards (filter-stage all-cards "ready") :wip 3}
                {:label :deck :cards
                 (filter-stage all-cards "deck")}])
  
  

  columns
 (specialty (nth developers 0))
  (filter #(= (:label %) (specialty (nth developers 0))) columns)

  (->> columns
       pull
      ;;  pull
      ;;  pull
      ;;  pull
       (filter #(not= (:label %) :deck))
       (map #(map (fn [c] [(:StoryId c) (:stage c)]) (:cards %))))

  (pprint/pp)
  ;
  )
