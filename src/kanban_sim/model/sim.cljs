(ns kanban-sim.model.sim
  (:require [cljs.pprint :as pprint]
            [clojure.string :as string]
            [kanban-sim.model.board :refer [create-columns pull-cards]]
            [kanban-sim.model.card :refer [done? work-on-card]]
            [kanban-sim.model.cards :refer [all-cards cards->map]]
            [kanban-sim.model.members :refer [developers specialty]]
            [kixi.stats.core :refer [standard-deviation mean]]
            [redux.core :refer [fuse]]))



(defn assign-to-card [card developer]
  (let [developers (:developers card)]
    (if developers
      (update card :developers conj developer)
      (assoc card :developers [developer]))))


;;
;; assign each developer to a random card 
;; first looks in their specialty
;;
(defn find-random-card-to-work [developer cards]
  (let [specialty-cards (filter #(= (:stage %) (specialty developer)) cards)]
    (rand-nth (if (> (count specialty-cards) 0)
                specialty-cards
                (filter #(not= (:stage %) (specialty developer)) cards)))))

(defn assign-developer [developer cards]
  (let [cards-map (cards->map cards)
        story-id (:StoryId (find-random-card-to-work developer cards))
        assigned-card (assign-to-card (cards-map story-id) developer)]
    (into [] (vals (assoc cards-map story-id assigned-card)))))

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

(defn update-days [day cards]
  (map #(update-card-day-ready day (update-card-day-deployed day %)) cards))

(defn develop-cycle [day developers cards]
  (->> cards
       (assign developers)
       (map work-on-card)
       pull-cards
       (update-days day)))

(defn log-cards [cards]
  (print "LOG")
  (pprint/pprint (->> cards
                      create-columns
                      (filter #(not= (:label %) :deck))
                      (map #(map
                             (fn [c] [(:Name c) (:Value c) (:stage c) (:DayReady c) (:DayDeployed c) (done? c) (:developers c)])
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

(defn start-sim [day-nr financial developers cards]
  (let [unfinished-cards (count (filter #(not (done? %)) cards))]
    (if (and (< day-nr 23) (> unfinished-cards 0))
      (let [;; _ (println "Start")
            worked-cards (develop-cycle day-nr developers cards)]
        ;; (when (= day-nr 22)
        ;;   (println "AT BEGINNING OF DAY" day-nr)
        ;;   (log-cards cards)
        ;;   (println financial)
        (start-sim (inc day-nr) (update-financial day-nr financial worked-cards) developers worked-cards))
      ;; (
      ;;  let [_ (log-cards cards)
      ;;       ]
      ;;   (println "DECK " (map :Name (filter #(= (:stage %) "deck") cards))))
      financial
      ;; )
      )))

(comment
  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       all-cards)


  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign-developer (nth developers 0) all-cards))

  (map (fn [c] [(:StoryId c) (:stage c) (:developers c)])
       (assign developers all-cards))


;; -----------------

  developers


  (filter #(= (:Name %) "S1") all-cards)

  (def stories-only (->> all-cards
                         (filter #(string/includes? (:Name %) "S"))
                         pull-cards))


  (->> stories-only log-cards)

  financial
  (start-sim start-day financial developers all-cards)
  (start-sim start-day financial developers stories-only)

  ;; statistics

  (time (->> (map (fn [n] (:revenue (start-sim start-day financial developers stories-only))) (range 100))
             (transduce identity (fuse {:mean mean :sd standard-deviation :min min :max max}))))


  ; -----------------------------

  (->> stories-only
       log-cards

       (develop-cycle 11 developers)
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


  (pprint/pp)
  ;
  )


