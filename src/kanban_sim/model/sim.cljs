(ns kanban-sim.model.sim
  (:require [cljs.pprint :as pprint]
            [clojure.string :as string]
            [kanban-sim.model.board :refer [create-columns pull-cards]]
            [kanban-sim.model.card :refer [done? estimate-work-left
                                           work-on-card work-to-do]]
            [kanban-sim.model.cards :refer [all-cards cards->map]]
            [kanban-sim.model.members :refer [developers]]
            [kanban-sim.model.policies :refer [find-card-with-more-work
                                               find-random-card-to-work]]
            [kixi.stats.core :refer [mean standard-deviation]]
            [redux.core :refer [fuse]]))



(defn assign-to-card [card developer]
  (let [developers (:developers card)]
    (if developers
      (update card :developers conj developer)
      (assoc card :developers [developer]))))



;;
(defn assign-developer [policy developer cards]
  (let [cards-map (cards->map cards)
        ;; story-id (:StoryId (find-random-card-to-work developer cards))
        ;; story-id (:StoryId (find-card-with-more-work developer cards))
        story-id (:StoryId ((:select-card-to-work policy) developer cards))
        assigned-card (assign-to-card (cards-map story-id) developer)]
    (into [] (vals (assoc cards-map story-id assigned-card)))))

(defn assign [policy developers cards]
  (let [[developer & devs] developers]
    (if developer
      (assign policy devs (assign-developer policy developer cards))
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

(defn update-days [day cards]
  (map #(update-card-day-ready day (update-card-day-deployed day %)) cards))

(defn develop-cycle [policy day developers cards]
  (->> cards
       (assign policy developers)
       (map work-on-card)
       pull-cards
       (update-days day)))

(defn log-cards [cards]
  (print "LOG")
  (pprint/pprint (->> cards
                      create-columns
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
      financial
      ;; )
      )))

(def stories-only (->> all-cards
                       (filter #(string/includes? (:Name %) "S"))
                       pull-cards))


(comment
  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       all-cards)


  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign-developer {:select-card-to-work find-card-with-more-work} (nth developers 0) all-cards))

  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign {:select-card-to-work find-card-with-more-work} developers all-cards))


;; -----------------

  developers



  (filter #(= (:Name %) "S1") all-cards)



  (->> stories-only log-cards)

  financial
  (start-sim
   {:select-card-to-work find-card-with-more-work}
   start-day financial developers all-cards)

  (start-sim
   {:select-card-to-work find-card-with-more-work}
   start-day financial developers stories-only)

  ;; statistics

  (time (->> (map (fn [n] (:revenue (start-sim
                                     {:select-card-to-work find-random-card-to-work}
                                     start-day financial developers stories-only))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))


  (time (->> (map (fn [n] (:revenue (start-sim
                                     {:select-card-to-work find-card-with-more-work}
                                     start-day financial developers stories-only))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))


  ; -----------------------------

  (find-card-with-more-work (first developers) [(first stories-only)])
  (first developers) ; analyst


  (->> stories-only
       log-cards
       (find-card-with-more-work (first developers)))

  (filter #(= (:stage %) "analysis") stories-only)

  (->> stories-only
       log-cards

       (develop-cycle {:select-card-to-work find-card-with-more-work} 11 developers)
       log-cards

      ;;  (develop-cycle 12 developers)
      ;;  log-cards

      ;;  (develop-cycle 13 developers)
      ;;  log-cards

       create-columns
       (filter #(not= (:label %) :deck))
       (map #(map
              (fn [c] [(:StoryId c) (:stage c) (done? c) (:developers c)])
              (:cards %))))


  (->> stories-only
       log-cards
       (assign {:select-card-to-work find-card-with-more-work} developers)
       log-cards
       ((fn [cars] nil)))


  (pprint/pp)
  ;
  )



