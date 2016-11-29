(ns aima.agents.environment)

(defprotocol Sensor
  (sense [sensor agent env]))

(defmulti actuate (fn [action agent env]
                    action))

(defprotocol Agent
  (location [agent])
  (execute [agent env]))

(defprotocol Environment
  (run [env])
  (step [env])
  (done? [env]))
