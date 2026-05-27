#!/usr/bin/env -S clojure -M

(defn split-passwd [f]
  (when-let [r (clojure.java.io/reader f)]
    (map #(last (clojure.string/split % #":"))
         (line-seq r))))

(defn -main [& _]
  (doseq [[k v] (frequencies (split-passwd "passwd"))]
    (println k ":" v)))

(apply -main *command-line-args*)
