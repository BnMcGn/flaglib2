;; This test runner is intended to be run from the command line
(ns flaglib2.test-runner
  (:require
   ;; require all the namespaces that you want to test
   [flaglib2.core-test]
   [flaglib2.excerpts-test]
   [flaglib2.excerpt-search-test]
   [flaglib2.misc-test]
   [flaglib2.mood-test]
   [flaglib2.hixer-test]
   [flaglib2.fabricate-test]
   [flaglib2.posters-test]
   [flaglib2.form-test]
   [flaglib2.ipfs-test]

   [figwheel.main.testing :refer [run-tests-async]]
   [cljs-test-display.core]))




(defn -main [& args]
  (run-tests-async 5000 (cljs-test-display.core/init!)))
