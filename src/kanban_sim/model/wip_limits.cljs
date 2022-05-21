(ns kanban-sim.model.wip-limits)

(def wip-limits [5 3 5 3])

(defn ready-wip-limit [limits]
  (nth limits 0))

(defn analysis-wip-limit [limits]
  (nth limits 1))

(defn development-wip-limit [limits]
  (nth limits 2))

(defn test-wip-limit [limits]
  (nth limits 3))