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
               (misc/reformat-urls-lists-simple (:fetchers/author-urls db))
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






(defn suggest-button [itm]
  [rc/button
   :label itm
   :on-click (fn [] (rf/dispatch [::enter-search itm]))])

(defn display-urls-in-categories []
  (let [labels {:rooturls "Previous Targets"
                :references "Previous References"
                :replies "References from replies to your posts"}
        aurls @(rf/subscribe [:flaglib2.fetchers/author-urls])]
    [rc/v-box
     :children
     (reduce into
             (for [[cat items] aurls
                   :when (seq items)]
               (into [[rc/box :child (or (get labels cat) "")]]
                     (for [itm items]
                       [suggest-button itm]))))]))

(defn display-searched-urls []
  (let [aurls @(rf/subscribe [::url-search-results])]
    [rc/v-box
     :children
     (for [itm aurls]
       [suggest-button (:url itm)])]))

(defn make-opinion []
  (let [search @(rf/subscribe [::search])
        search-res @(rf/subscribe [::url-search-results])]
    [:div
     [rc/input-text
      :placeholder "Target URL or search terms"
      :model search
      :on-change (fn [ev] ev (rf/dispatch [::enter-search ev]))]
     (if search-res
       [display-searched-urls]
       [display-urls-in-categories])]))

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
           [:mount-registered]]})))




;;Decisioner: what to do if we don't have text

(defn proceed-button [& alternate]
  [rc/button
   :label (or alternate "Next")
   :on-click (fn [] (rf/dispatch [:opine]))])

(defn review-text-button []
  [rc/button
   :label "Review Text"
   :on-click (fn [] (rf/dispatch [::review-text]))])

(defn supply-text-button []
  [rc/button
   :label "Supply Text"
   :on-click (fn [] (rf/dispatch [::supply-text]))])

(defn target-decision []
  (let [selection @(rf/subscribe [::selection])
        factors (and selection @(rf/subscribe [:target-decision selection]))]
    (cond
      (not factors) []
      (and (:reviewed factors) (:available factors))
      [rc/v-box [proceed-button]]
      (:available factors)
      [:div
       [:h3 "Unreviewed article text"]
       [:ul
        [:li "The text of this article has not yet been reviewed for tidyness"]
        [:li "You may review it for legibility and post an edited version using the 'Review Text' button"]
        [:li "You may also skip to posting flags on the article"]]
       [rc/v-box [review-text-button] [proceed-button "Skip to Flagging"]]]
      (= (:status factors) "wait")
      [:div
       [:h3 "Waiting for text extraction"]
       [:ul
        [:li "Target text is being fetched and extracted"]
        [:li "You may supply the article text manually"]
        [:li "Skip to posting if you don't need the article text"]]
       [rc/v-box [supply-text-button] [proceed-button "Skip to Flagging"]]]
      (= (:status factors) "failure")
      [:div
       [:h3 "Text from article at " (misc/url-domain selection) " is not currently available."]
       [:h4 "Reason: " (:message factors)]
       [:ul
        [:li "You may supply the article text manually"]
        ;;FIXME
        ;;[:li "If the same text is available at another URL, please indicate the alternative with the SameThing flag."]
        [:li "You might not need the article text, for example, if you aren't using excerpts."]]
       [rc/v-box [supply-text-button] [proceed-button "Flag Anyways"]]])))
