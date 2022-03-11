(ns flaglib2.fabricate
  (:require
   [goog.dom :as gdom]
   [re-frame.core :as rf]
   [reagent.core :as r]
;   [day8.re-frame.http-fx]
;   [ajax.core :as ajax]
;   [clojure.string :as string]
;   [clojure.walk :as walk]
   [flaglib2.fetchers :as fetchers]
   [flaglib2.ipfs :as ip]
   [flaglib2.misc :as misc]
   [cljsjs.fuse :as fuse]
   [re-com.core :as rc]))

(def fabricate-hooks
  {:fetchers/received-author-urls [::get-stuff-for-author-urls]
   ::search-provided [::get-stuff-for-selection]})

(rf/reg-event-fx
 ::get-stuff-for-author-urls
 ;;We assume that author-urls are already in ipfs. No check.
 (fn [{:keys [db]} _]
   {:dispatch [:load-rooturls
               (fetchers/reformat-urls-lists-simple (:fetchers/author-urls db))
               :no-text true]}))

(rf/reg-event-db
 ::enter-search
 [fetchers/hook-inserter]
 (fn [db [_ search]]
   (let [ndb (assoc db ::search search)]
     (if (misc/url? search)
       (assoc ndb ::selection search)
       ndb))))

;;FIXME: will load all of url data. No way to check if we already have it.
(rf/reg-event-fx
 ::get-stuff-for-selection
 (fn [{:keys [db]} _]
   {:dispatch [:load-rooturls [(::selection db)] :gently true]}))






(rf/reg-sub
 ::url-search-results
 (fn [db _]
   (let [search (::search db)
         aurls (:fetchers/author-urls db)]
     (when (and search aurls)
       (let [fus (fuse/fuse (fetchers/reformat-urls-lists aurls)
                            (clj->js {:include-score true :keys (list :url)}))]
         (fus.search search))))))

(rf/reg-sub
 ::search
 (fn [db _]
   (::search db)))






(defn make-opinion []
  (let [search @(rf/subscribe [::search])]
    [:div
     [rc/input-text
      :placeholder "Enter a Target URL or search terms"
      :model search
      :on-change (fn [ev] (println ev) (rf/dispatch [::enter-search ev.target.value]))]]))

(rf/reg-event-fx
 :make-opinion
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :target])]
     {:db (assoc db :root-element make-opinion)
      :fx [ [:dispatch
             (if target
               [::enter-search target]
               [:fetchers/load-author-urls])]
           [:dispatch [:add-hooks fabricate-hooks]]
           ;;FIXME: is this the right place?
           [:dispatch [:mount-registered]]]})))




