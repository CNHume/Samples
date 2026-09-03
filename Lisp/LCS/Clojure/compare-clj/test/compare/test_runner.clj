(ns compare.test-runner
  (:require [clojure.test :as t]
            [compare.core-test]))

(defn -main [& _args]
  (let [result (t/run-tests 'compare.core-test)
        ok? (and (zero? (:fail result)) (zero? (:error result)))]
    (shutdown-agents)
    (System/exit (if ok? 0 1))))
