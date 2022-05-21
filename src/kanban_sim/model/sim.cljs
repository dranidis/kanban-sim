(ns kanban-sim.model.sim
  (:require [cljs.pprint :as pprint]
            [clojure.string :as string]
            [kanban-sim.model.board :refer [create-columns
                                            est-development-done pull-cards]]
            [kanban-sim.model.card :refer [cycle-time done? estimate-work-left
                                           make-card work-on-card work-to-do]]
            [kanban-sim.model.cards :refer [all-cards]]
            [kanban-sim.model.members :refer [developers]]
            [kanban-sim.model.policies :refer [assign assign-developer
                                               assign-to-card
                                               select-card-with-least-work select-card-with-least-work-mixed-considering-done
                                               select-card-with-more-work select-card-with-more-work-considering-done select-random-card-to-work]]
            [kanban-sim.model.wip-limits :refer [wip-limits]]
            [kixi.stats.core :refer [mean standard-deviation]]
            [redux.core :refer [fuse]]))


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

(defn update-days [day cards]
  (map #(update-card-day-ready day (update-card-day-deployed day %)) cards))

(defn develop-cycle [policy day developers cards]
  (->> cards
       (assign policy developers)
       (map work-on-card)
       (pull-cards (:wip-limits policy))
       (update-days day)))

(defn log-cards [wip-limits cards]
  (print "LOG")
  (pprint/pprint (->> cards
                      (create-columns wip-limits)
                      (filter #(not= (:label %) :deck))
                      (map #(map
                             (fn [c] [(:Name c) "DO" (work-to-do c)
                                      "EST" (:estimated-work-left (estimate-work-left c))
                                      (:stage c) (:DayReady c) (:DayDeployed c) (done? c) (:developers c)])
                             (:cards %)))))
  (println)
  cards)

(def start-day 9)
(def financial {:subs 0 :revenue 0})

(defn update-subscribers [day subs card]
  (if (and (= (:stage card) "deployed")
           (< (- day (:DayDeployed card)) 3))
    (+ subs (:Value card))
    subs))

(defn billing-cycle-subs [day cards]
  (reduce (partial update-subscribers day) 0 cards))

(defn update-financial [day financial cards]
  (if (zero? (rem day 3))
    (let [new-subs (billing-cycle-subs day cards)
          ;; _ (println "New subs: " new-subs)
          total-subs (+ (:subs financial) new-subs)
          total-revenue (+ (:revenue financial) (* total-subs 100))]
      (-> financial
          (assoc :subs total-subs)
          (assoc :revenue total-revenue)))
    financial))

(defn start-sim [policy day-nr financial developers cards]
  (let [unfinished-cards (count (filter #(not (done? %)) cards))]
    (if (and (< day-nr 23) (> unfinished-cards 0))
      (let [;; _ (println "Start")
            worked-cards (develop-cycle policy day-nr developers cards)]
        ;; (when (= day-nr 22)
        ;;   (println "AT BEGINNING OF DAY" day-nr)
        ;;   (log-cards cards)
        ;;   (println financial)
        (start-sim policy (inc day-nr) (update-financial day-nr financial worked-cards) developers worked-cards))
      ;; (
      ;;  let [_ (log-cards cards)
      ;;       ]
      ;;   (println "DECK " (map :Name (filter #(= (:stage %) "deck") cards))))
      {:financial financial :cards cards}
      ;; )
      )))

(def stories-only (->> all-cards
                       (filter #(string/includes? (:Name %) "S"))
                       (pull-cards wip-limits)))


(comment

  (update-card-day-deployed 5 (assoc (first stories-only) :stage "deployed"))
  (update-card-day-ready 5 (assoc (assoc (first stories-only) :stage "ready") :DayReady 0))


  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       all-cards)


  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign-developer {:select-card-to-work select-card-with-more-work} (nth developers 0) all-cards))

  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign {:select-card-to-work select-card-with-more-work} developers all-cards))


;; -----------------

  (->> stories-only
       (log-cards wip-limits)
       ((fn [_] nil)))

  financial
  (start-sim
   {:select-card-to-work select-card-with-more-work}
   start-day financial developers all-cards)

  (start-sim
   {:select-card-to-work select-card-with-more-work}
   start-day financial developers stories-only)
  
  (start-sim
   {:wip-policy true
    :select-card-to-work select-card-with-more-work}
   start-day financial developers stories-only)

  ;; statistics

  (time (->> (map (fn [_] (:revenue (:financial (start-sim
                                                 {:select-card-to-work select-random-card-to-work
                                                  :wip-limits wip-limits}
                                                 start-day financial developers stories-only)))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))


  (time (->> (map (fn [_] (:revenue (:financial (start-sim
                                                 {:select-card-to-work select-card-with-more-work
                                                  :wip-limits wip-limits}
                                                 start-day financial developers stories-only)))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))


  (time (->> (map (fn [_] (:revenue (:financial (start-sim
                                                 {:select-card-to-work select-card-with-least-work
                                                  :wip-limits wip-limits}
                                                 start-day financial developers stories-only)))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))

  (time (->> (map (fn [_] (:revenue (:financial (start-sim
                                                 {:select-card-to-work select-card-with-least-work-mixed-considering-done
                                                  :wip-limits wip-limits}
                                                 start-day financial developers stories-only)))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))

  (time (->> (map (fn [_] (:revenue (:financial (start-sim
                                                 {:select-card-to-work select-card-with-more-work-considering-done
                                                  :wip-limits wip-limits}
                                                 start-day financial developers stories-only)))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))


  (map #(str "<" (:DayReady %) "-" (:DayDeployed %) "=" (cycle-time %) ">")
       (filter (fn [c] (= (:stage c) "deployed"))
               (:cards (start-sim
                        {:select-card-to-work select-card-with-more-work-considering-done}
                        start-day financial developers stories-only))))

  (->> (map cycle-time
            (filter (fn [c] (= (:stage c) "deployed"))
                    (:cards (start-sim
                             {:select-card-to-work select-card-with-more-work-considering-done}
                             start-day financial developers stories-only))))
       (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max})))




  ; -----------------------------

  (select-card-with-more-work (first developers) [(first stories-only)])
  (first developers) ; analyst


  (->> stories-only
       (log-cards wip-limits)
       (select-card-with-more-work (first developers)))

  (filter #(= (:stage %) "analysis") stories-only)

  (->> stories-only
       (log-cards wip-limits)

       (develop-cycle {:select-card-to-work select-card-with-more-work} 11 developers)
       (log-cards wip-limits)

      ;;  (develop-cycle 12 developers)
      ;;  (log-cards wip-limits)

      ;;  (develop-cycle 13 developers)
      ;;  (log-cards wip-limits)

       (create-columns wip-limits)
       (filter #(not= (:label %) :deck))
       (map #(map
              (fn [c] [(:StoryId c) (:stage c) (done? c) (:developers c)])
              (:cards %))))


  (->> stories-only
       (log-cards wip-limits)
       (assign {:select-card-to-work select-card-with-least-work-mixed-considering-done} developers)
       (log-cards wip-limits)
       (map work-on-card)
       (log-cards wip-limits)
       (pull-cards wip-limits)
       (log-cards wip-limits)
       (update-days 6)

       (assign {:select-card-to-work select-card-with-least-work-mixed-considering-done} developers)
       (log-cards wip-limits)
       (map work-on-card)
       (log-cards wip-limits)
       (pull-cards wip-limits)
       (log-cards wip-limits)
       (update-days 7)

       (assign {:select-card-to-work select-card-with-least-work-mixed-considering-done} developers)
       (log-cards wip-limits)
       (map work-on-card)
       (log-cards wip-limits)
       (pull-cards wip-limits)
       (log-cards wip-limits)
       (update-days 8)



       (map #(:DayDeployed %))

      ;;  ((fn [_] nil))
       )

  (def cards [(make-card "T1" "test" 10)
              (assign-to-card (make-card "T2" "test" 8) {:Role "tester"})
              (assign-to-card (make-card "T3" "test" 5) {:Role "tester"})
              (make-card "D1" "development-done" 12)
              (assign-to-card (make-card "D2" "development" 8) {:Role "developer"})
              (make-card "D3" "development" 10)
              (make-card "D4" "development" 12)
              (make-card "D5" "development" 8)
              ;
              ])

  


  (->> cards
       (log-cards wip-limits)
       (develop-cycle {:wip-policy true
                       :wip-limits wip-limits
                       :select-card-to-work select-card-with-more-work} 11 developers)
       )


  (pprint/pp)
  ;
  )





