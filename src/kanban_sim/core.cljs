(ns kanban-sim.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [kanban-sim.events :as events]
   [kanban-sim.views :as views]
   [kanban-sim.config :as config]
   ))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defn init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))

(comment
  (init)
  ;
  )