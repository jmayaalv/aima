(ns aima.agents.vacuum
  (:require [aima.agents.environment :as env :refer [Environment Agent Sensor]]
            [clojure.pprint :as pprint]))

;;; Private
(defn cell [{:keys [world] :as env} location]
  (get-in world (flatten [location])))

(defn agent-location [{:keys [world] :as env} {:keys [id]}]
  (let [location (some (fn [cell]
                         (let [agents (:agents cell)]
                           (when (some #{id} agents)
                             (:location cell))))
                       world)]
    (if (seq? location)
      location
      [location])))

;;; Sensors

(defrecord LocationSensor []
  Sensor
  (sense [sensor agent env]
    (agent-location env agent)))

(defrecord DirtSensor []
  Sensor
  (sense [sensor agent env]
    (let [location (agent-location env agent)
          cell (cell env location)]
      (when (:dirt cell)
        :dirty))))

;;; Actuators

(defn move-agent [agent env new-location]
  (let [location (env/location agent)
        agent-id (:id agent)]
    (-> env
        (update-in (vec (flatten [:world location :agents])) (fn [agents]
                                                               (remove #(= % agent-id)
                                                                       agents)))
        (update-in (vec (flatten [:world new-location :agents])) conj agent-id)
        )))

(defmethod env/actuate :suck [_ agent  env]
  (let [location (env/location agent)]
    (update-in env (vec (flatten [:world location])) assoc :dirt false)))

(defmethod env/actuate :left [_ agent  env]
  (let [location (env/location agent)
        move-to (update location 0 dec)]
    (move-agent agent env move-to)))

(defmethod env/actuate :right [_ agent  env]
  (let [location (env/location agent)
        move-to (update location 0 inc)]
    (move-agent agent env move-to)))

;;; Agent

(defrecord VacuumAgent [id sensors agent-fn]
  Agent
  (location [agent]
    (let [[location _] (last (:percept-sequence agent))]
      location))

  (execute [agent env]
    (let [percepts (for [sensor (:sensors agent)]
                     (env/sense sensor agent env))
          agent (update agent :percept-sequence conj percepts)
          action (agent-fn percepts)]
      (pprint/pprint action)
      (env/actuate action agent env))))

(defn make-vacumm-agent [id fn]
  (->VacuumAgent id [(->LocationSensor) (->DirtSensor)] fn))

(defn simple-agent-fn [[location dirt?]]

  (cond
    dirt? :suck
    (= 0 (first location)) :right
    (= 1 (first location)) :left
    :else :no-op))

;;; Environemnt

(defn step [env agent]
  (env/execute agent env))

(defrecord SimpleEnvironment [agents world]
  Environment
  (done? [{:keys [world] :as env}]
    (not (some #(:dirt %)
               world)))

  (step [{:keys [agents] :as env}]
    (last (for [agent agents]
       (step env agent)))) ;; There is a problem when there are multiple agents

  (run [{:keys [agents world] :as env}]
    (if (env/done? env)
      env
      (-> (env/step env)
          env/run))))

(defn make-environment [agents world]
  (->SimpleEnvironment agents world))

(def simple-world
  [{:dirt false
    :agents [:vacuum1]
    :location 0}
   {:dirt true

    :location 1}])

(defn make-simple-environment []
  (make-environment [(make-vacumm-agent :vacuum1 simple-agent-fn)] simple-world))
