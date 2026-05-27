#!/usr/bin/env clojure

(defn split-passwd [f]
  ;; Open file inside with-open block so file streams don't leak
  (with-open [r (clojure.java.io/reader f)]
    ;; doall forces evaluation of the lazy map before the file stream closes
    (doall (map #(last (clojure.string/split % #":"))
                (line-seq r)))))

(defn -main [& _]
  (doseq [[k v] (frequencies (split-passwd "passwd"))]
    (println k ":" v)))

;; Check if arguments exist, otherwise fall back to empty args
(apply -main *command-line-args*)
