(ns flaglib2.fabricate-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]

     [clojure.string :as string]

     [re-frame.alpha :as rf]
     [re-frame.flow.alpha :as flow]
     [day8.re-frame.test :as rf-test]

     [flaglib2.init :as init]
     [flaglib2.misc :as misc]
     [flaglib2.urlgrab :as ug]
     [flaglib2.fabricate :as fab]))

(deftest current-opinion
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters
                 nil
                 {:excerpt "ick bro"
                  :offset 2
                  :flag :custodial-retraction
                  :target "http://google.com/"}])
   (let [opinions @(rf/subscribe [:current-opinion])
         opinion (:opinion opinions)]
     (is (= "ick bro" (:excerpt opinion)))
     (is (= 2 (:excerpt-offset opinion)))
     (is (= :custodial-retraction (:flag opinion)))
     ;;Target does not automatically get set from :default
     (is (not (:target opinion))))
   (rf/dispatch [::ug/enter-search [::fab/specify-target] "http://woogle.com/"])
   (rf/dispatch [::fab/set-flag :custodial-out-of-date])
   (rf/dispatch [::fab/set-excerpt ["ox jum" 0]])
   (rf/dispatch [::fab/set-comment "is a"])
   (rf/dispatch [::fab/set-supplied-text "box jumbo"])
   (rf/dispatch [::fab/set-supplied-title "brown fox"])
   (let [opinions @(rf/subscribe [:current-opinion])
         opinion (:opinion opinions)]
     (is (= "ox jum" (:excerpt opinion)))
     (is (= 0 (:excerpt-offset opinion)))
     (is (= :custodial-out-of-date (:flag opinion)))
     (is (= "http://woogle.com/" (:target opinion)))
     (is (= "box jumbo" (:alternate opinions)))
     (is (= "brown fox" (:alt-title opinions))))))

