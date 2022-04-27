(ns flaglib2.subscriptions
  (:require
   [re-frame.core :as rf]
   [flaglib2.misc :as misc]
   [cljsjs.fuse :as fuse]

   [clojure.walk :as walk]))


(rf/reg-sub
 :server-parameters
 (fn [db _]
   (:server-parameters db)))

(rf/reg-sub
 :root-element
 (fn [db _]
   (:root-element db)))

(rf/reg-sub
 :url-search-results
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
 :flaglib2.fabricate/selection
 (fn [db _]
   (:flaglib2.fabricate/selection db)))

(rf/reg-sub :flaglib2.fabricate/review-text :-> :flaglib2.fabricate/review-text)


(rf/reg-sub
 :flaglib2.fetchers/author-urls
 (fn [db _]
   (:flaglib2.fetchers/author-urls db)))


(rf/reg-sub
 :warstats-store
 (fn [db [_ key]]
   (get-in db [:warstats-store key])))

(rf/reg-sub
 :text-store
 (fn [db [_ key]]
   (get-in db [:text-store key])))

(rf/reg-sub
 :title-store
 (fn [db [_ key]]
   (get-in db [:title-store key])))

(rf/reg-sub
 :opinion-store
 (fn [db [_ key]]
   (get-in db [:opinion-store key])))

(rf/reg-sub
 :text-status
 (fn [db [_ key]]
   (get-in db [:text-status key])))


(rf/reg-sub
 :target-decision
 (fn [[_ target]]
   [(rf/subscribe [:warstats-store target])
    (rf/subscribe [:text-store target])
    (rf/subscribe [:text-status target])])
 (fn [[warstat text status] _]
   (let [have-text (and text (:text text))
         responses (and warstat (not (zero? (:replies-total warstat))))]
     {:status (cond (and responses have-text) :reviewed
                    have-text :available
                    :else (walk/keywordize-keys (:status status)))
      :message (and status (:message status))})))

(rf/reg-sub :flaglib2.fabricate/flag :-> :flaglib2.fabricate/flag)
(rf/reg-sub
 :flaglib2.fabricate/flag-or-default
 :<- [:flaglib2.fabricate/flag]
 :<- [:server-parameters]
 (fn [[flag params] _]
   (or flag (:flag params))))

