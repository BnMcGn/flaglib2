(ns flaglib2.subscriptions
  (:require
   [re-frame.core :as rf]
   [flaglib2.misc :as misc]
   [cljsjs.fuse :as fuse]
   ))


(rf/reg-sub
 :server-parameters
 (fn [db _]
   (:server-parameters db)))

(rf/reg-sub
 :root-element
 (fn [db _]
   (:root-element db)))

(rf/reg-sub
 ::url-search-results
 (fn [db _]
   (let [search (::search db)
         aurls (:fetchers/author-urls db)]
     (when (and search aurls)
       (let [fus (fuse/fuse (misc/reformat-urls-lists aurls)
                            (clj->js {:include-score true :keys (list :url)}))]
         (fus.search search))))))

(rf/reg-sub
 :flaglib2.fabricate/search
 (fn [db _]
   (:flaglib2.fabricate/search db)))


(rf/reg-sub
 :flaglib2.fetchers/author-urls
 (fn [db _]
   (:flaglib2.fetchers/author-urls db)))













