(ns flaglib2.posters-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]

     [clojure.string :as string]

     [flaglib2.misc :as misc]
     [flaglib2.posters :as posters]))

(deftest contains-tt-tag
  (is (posters/contains-tt-tag? "#(target-text)"))
  (is (not (posters/contains-tt-tag? "#(targut-title)")))
  (is (not (posters/contains-tt-tag? " #(suggest-target-title)"))))

(deftest ttify-opinion
  (let [topin (posters/ttify-opinion {:comment "Replacement"} "title" true)]
    (is (string/starts-with? "#(suggest-target-title)" (:comment topin)))
    (is (= :custodial-blank (:flag topin))))
  (is (thrown? js/Error
               (posters/ttify-opinion {:flag :negative-disagree} "text" false)))
  (is (thrown? js/Error
               (posters/ttify-opinion) {:comment "#(target-text)"} "something" true)))

(deftest post-opinion
  (let [db {::posters/opinion-status {:success :non-failed}}
        effects (misc/fake-event
                 [:post-opinion {:alternate "other" :target "http://google.com/"
                                 :flag :positive-like}]
                 db)
        evt (second (first (filter identity (:fx effects))))

        effects2 (misc/fake-event evt {})
        xhrio (:http-xhrio effects2)]
    (is evt)
    (is xhrio)
    (is "http://google.com/" (get-in xhrio [:params :target]))
    (is (= [::posters/posted-opinion-to-server ::posters/alternate-status]
           (:on-success xhrio)))))
