(ns flaglib2.posters-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]

     [clojure.string :as string]

     [re-frame.alpha :as rf]
     [re-frame.flow.alpha :as flow]

     [flaglib2.fabricate :as fab]
     [flaglib2.urlgrab :as ug]
     [flaglib2.misc :as misc]
     [flaglib2.posters :as posters]))

(deftest contains-tt-tag
  (is (posters/contains-tt-tag? "#(target-text)"))
  (is (not (posters/contains-tt-tag? "#(targut-title)")))
  (is (not (posters/contains-tt-tag? " #(suggest-target-title)"))))

(deftest ttify-opinion
  (let [topin (posters/ttify-opinion {:comment "Replacement"} "title" true)]
    (is (string/starts-with? (:comment topin) "#(suggest-target-title)"))
    (is (= :custodial-blank (:flag topin))))
  (is (thrown? js/Error
               (posters/ttify-opinion {:flag :negative-disagree} "text" false)))
  (is (thrown? js/Error
               (posters/ttify-opinion {:comment "#(target-text)"} "something" true))))

(deftest post-opinion
  (let [db {::posters/opinion-status {:success :non-failed}
            ::fab/specify-target
            {::ug/modified-selection "http://google.com/"}
            ::fab/flag :positive-like
            ::fab/supplied-text "other"}
        ctx (reduce flow/run {:effects {:db db}} (flow/topsort @flow/flows))
        db (get-in ctx [:effects :db])
        effects (misc/fake-event
                 [:post-opinion ]
                 db)
        evt (second (first (filter identity (:fx effects))))

        effects2 (misc/fake-event evt {})
        xhrio (:http-xhrio effects2)]
    (is evt)
    (is xhrio)
    (is "http://google.com/" (get-in xhrio [:params :target]))
    (is (= [::posters/posted-opinion-to-server [::posters/alternate-status]]
           (:on-success xhrio)))))
