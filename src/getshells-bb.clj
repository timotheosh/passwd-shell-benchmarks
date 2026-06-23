#!/usr/bin/env bb 

(defn get-shell [line]
  (let [idx (clojure.string/last-index-of line ":")]
    (subs line (inc idx))))

(defn -main [& _]
  (with-open [r (clojure.java.io/reader "passwd")]
    (doseq [[k v] (frequencies (map get-shell (line-seq r)))]
      (println k ":" v))))

;; Check if arguments exist, otherwise fall back to empty args
(apply -main *command-line-args*)
