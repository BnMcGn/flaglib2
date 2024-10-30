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

(deftest target-adjust
  (let [res (misc/target-adjust "https://web.archive.org/web/20120424172252/https://www.nytimes.com/2012/04/23/business/media/tv-news-corrects-itself-just-not-on-the-air.htm")]
    (is (:message res))
    (is (= 97 (count (:adjusted res)))))
  (let [res (misc/target-adjust "https://warflagger.net/target/43")]
    (is (:message res))
    (is (not (:adjusted res))))
  (let [res (misc/target-adjust "https://warflagger.com/o/pnnkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge")]
    (is (:message res))
    (is (misc/iid? (:adjusted res)))))
