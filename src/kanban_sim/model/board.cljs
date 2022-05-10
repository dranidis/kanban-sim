(ns kanban-sim.model.board 
  (:require [clojure.pprint :as pprint]))


;;
;; pull using some strategy
;;
(defn remove-card [from-col]
  [(first from-col) (into [] (rest from-col))])

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
           (>  (count (:cards second-col)) 0))
    (let [[from-upd to-upd] (pull-card (:cards second-col) (:cards first-col))]
      (pull-from-to (assoc second-col :cards from-upd) (assoc first-col :cards to-upd)))
    [second-col first-col]))

(defn pull [columns]
  (let [[first-col second-col & rest-cols] columns]
    (if (nil? second-col)
      [first-col]
      (let [[second-upd first-upd] (pull-from-to second-col first-col)]
        (into [first-upd] (pull (into [second-upd] rest-cols)))))))


(comment
  (def columns [{:label :deployed :cards []}
                {:label :test :cards [1 2 3] :wip 2}
                {:label :development :cards [4 5] :wip 5}
                {:label :analysis :cards [6] :wip 4}
                {:label :ready :cards [7 8 9] :wip 3}
                {:label :deck :cards [10 11 12 13 14]}])
  columns

  (pull columns)
  (pull (pull columns))
  (pull (pull (pull columns)))
  (pull (pull (pull (pull columns))))
  (pull (pull (pull (pull (pull columns)))))
  (pull (pull (pull (pull (pull (pull columns))))))
  (pull (pull (pull (pull (pull (pull (pull columns)))))))


  (pprint/pp)
  ;
  )