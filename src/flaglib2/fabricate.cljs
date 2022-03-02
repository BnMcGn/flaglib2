(ns flaglib2.fabricate
  (:require
   [goog.dom :as gdom]
   [re-frame.core :as rf]
;   [day8.re-frame.http-fx]
;   [ajax.core :as ajax]
;   [clojure.string :as string]
;   [clojure.walk :as walk]
   [flaglib2.fetchers :as fetchers]
   [flaglib2.ipfs :as ip]
   [flaglib2.misc :as misc]
   [cljsjs.fuse :as fuse]))

(def fabricate-hooks
  {:fetchers/received-author-urls [::get-stuff-for-author-urls]})

(rf/reg-event-fx
 :make-opinion
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :target])]
     {:fx [ [:dispatch
             (if target
               [::search-provided target]
               [:fetchers/load-author-urls])]
           [:dispatch [:add-hooks fabricate-hooks]]]})))

(rf/reg-event-fx
 ::get-stuff-for-author-urls
 ;;We assume that author-urls are already in ipfs. No check.
 (fn [{:keys [db]} _]
   {:dispatch [:load-rooturls (:fetchers/author-urls db) :no-text no-text]}))

(rf/reg-event-db
 ::search-provided
 (fn [db [_ search]]
   (let [ndb (assoc db ::search search)]
     (if ))
   ))
