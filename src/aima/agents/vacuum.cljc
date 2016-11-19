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
  (print "move left")
  (-> env
   (assoc :position :left)
   (update :performance (fnil dec 0))))

(defmethod execute :right [env _]
  (print "move right")
  (-> env
   (assoc :position :right)
   (update :performance (fnil dec 0))))

(defmethod execute :clean [env _]
  (print "clean")
  (-> env
   (update-in [:world (get location (:position env))] :clean)
   (update :performance (fnil (partial + 10) 0))))

(defn sensor-fn [{:keys [position world] :as env}]
  [position (nth world (get location position))])

(defrecord VacuumEnvironment [agent world position]
  Environment
  (perceive [env senso-fn]
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

(defn make-vacuum-env [agent world position]
  (map->VacuumEnvironment {:agent agent
                           :world world
                           :position (or position :left)}))
