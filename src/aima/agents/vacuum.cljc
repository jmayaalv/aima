(ns aima.agents.vacuum
  (:require [aima.agents.environment :as env :refer [Environment Agent]]))


;; Actions: :left :right :clean

;; World is an array of n elements. possible values of the array, :dirty :clean

(defrecord SimpleVacuumAgent []
  Agent
  (execute [agent [pos status]]
    (if (= :dirty status)
        :clean
        (if (= pos :left)
          :right
          :left))))

(def location {:left 0
               :right 1})

(defmulti execute (fn [env action]
                    action))

(defmethod execute :left [env _]
  (assoc env :position :left))

(defmethod execute :right [env _]
  (assoc env :position :right))

(defmethod execute :clean [env _]
  (update-in env [:world (get location (:position env))] :clean))

(defn sensor-fn [{:keys [position world] :as env}]
  [position (nth world (get location position))])

(defrecord VacuumEnvironment [agent world position]
  Environment
  (perceive [env sensor-fn]
    (sensor-fn env))

  (actuate [env action]
    (execute env action))

  (step [env]
    (let [percepts (env/perceive env sensor-fn)
          agent (:agent env)
          action (env/execute agent percepts)]
      (env/actuate env action)))

  (run [env]
    (cond-> env
      (not (env/done? env)) env/step
      (not (env/done? env)) env/run))

  (done? [env]
    (not
     (some #(= % :dirty) (:world env)))))

(defn make-vacuum-env [agents world]
  (map->VacuumEnvironment {:agent (or agents [])
                           :world (or world [])
                           :position :left}))
