(ns flaglib2.misc-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [day8.re-frame.test :as rf-test]
     [flaglib2.misc :as misc]
     [re-frame.core :as rf]
     ))

(deftest call-something-event
  (rf-test/run-test-async
   (rf/reg-event-db
    ::res
    (fn [db [_ itm]]
      (assoc db ::res itm)))
   (rf/reg-sub ::res :-> ::res)
   (rf/reg-event-fx
    ::set-res
    ;;[misc/call-something]
    (fn [_ [_ itm]]
      {:call-something [::res itm]}))
   (rf/dispatch-sync [::set-res :that])
   (rf-test/wait-for [::res]
    (is (= :that @(rf/subscribe [::res]))))))

