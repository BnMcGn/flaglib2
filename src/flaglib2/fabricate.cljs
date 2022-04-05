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
   [flaglib2.stepper :as step]
   [cljsjs.fuse :as fuse]
   [re-com.core :as rc]))

(def fabricate-hooks
  {:flaglib2.fetchers/received-author-urls [::get-stuff-for-author-urls]
   ::enter-search [::get-stuff-for-selection]})

(rf/reg-event-fx
 ::get-stuff-for-author-urls
 ;;We assume that author-urls are already in ipfs. No check.
 (fn [{:keys [db]} _]
   {:dispatch [:load-rooturls
               (misc/reformat-urls-lists-simple (list (:flaglib2.fetchers/author-urls db)))
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
   {:dispatch [:load-rooturls [(::selection db)]]}))



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
  (let [aurls @(rf/subscribe [:url-search-results])]
    [rc/v-box
     :children
     (for [itm aurls]
       [suggest-button (:url itm)])]))



(defn specify-target []
  (let [search @(rf/subscribe [::search])
        search-res @(rf/subscribe [:url-search-results])]
    [:div
     [rc/input-text
      :placeholder "Target URL or search terms"
      :model search
      :on-change (fn [ev] ev (rf/dispatch [::enter-search ev]))]
     (if search-res
       [display-searched-urls]
       [display-urls-in-categories])]))

(defn specify-target-summary []
  (let [selection @(rf/subscribe [::selection])]
    [step/summary-button :specify-target "Target: "]))









;;Decisioner: what to do if we don't have text

(defn proceed-button [& [alternate]]
  [rc/button
   :label (or alternate "Next")
   :on-click (fn [] (rf/dispatch [:flaglib2.stepper/goto :opine]))])

(defn review-text-button []
  [rc/button
   :label "Review Text"
   :on-click (fn [] (rf/dispatch [:flaglib2.stepper/goto :review-text]))])

(defn supply-text-button []
  [rc/button
   :label "Supply Text"
   :on-click (fn [] (rf/dispatch [:flaglib2.stepper/goto :supply-text]))])


(defn target-decision []
  (let [selection @(rf/subscribe [::selection])
        factors (and selection @(rf/subscribe [:target-decision selection]))]
    (if factors
      (case (:status factors)
        :reviewed ""
        :available
        [:div
         [:h3 "Unreviewed article text"]
         [:ul
          [:li "The text of this article has not yet been reviewed for tidyness"]
          [:li "You may review it for legibility and post an edited version using the 'Review Text' button"]
          [:li "You may also skip to posting flags on the article"]]]
        :wait
        [:div
         [:h3 "Waiting for text extraction"]
         [:ul
          [:li "Target text is being fetched and extracted"]
          [:li "You may supply the article text manually"]
          [:li "Skip to posting if you don't need the article text"]]]
        :failure
        [:div
         [:h3 "Text from article at " (misc/url-domain selection) " is not currently available."]
         [:h4 "Reason: " (:message factors)]
         [:ul
          [:li "You may supply the article text manually"]
          ;;FIXME
          ;;[:li "If the same text is available at another URL, please indicate the alternative with the SameThing flag."]
          [:li "You might not need the article text, for example, if you aren't using excerpts."]]])
      "")))


(defn target-decision-buttons []
  (let [selection @(rf/subscribe [::selection])
        factors (and selection @(rf/subscribe [:target-decision selection]))]
    (if factors
      (case (:status factors)
        :reviewed
        (step/stepper-buttons
         :next nil
         :buttons [[proceed-button]])
        :available
        (step/stepper-buttons
         :next nil
         :buttons [[review-text-button] [proceed-button "Skip to Flagging"]])
        :wait
        (step/stepper-buttons
         :next nil
         :buttons [[supply-text-button] [proceed-button "Skip to Flagging"]])
        :failure
        (step/stepper-buttons
         :next nil
         :buttons [[supply-text-button] [proceed-button "Flag Anyways"]]))
     [])))

(rf/reg-event-db
 ::reset-review-text
 (fn [db _]
   (assoc db ::review-text (get-in db [:text-store (::selection db) :text]))))

(rf/reg-event-db
 ::set-review-text
 (fn [db [_ text]]
   (assoc db ::review-text text)))

(defn review-text []
  (let [
        text @(rf/subscribe [::review-text])]
    [:div
     [:h3 "Review article text for tidyness"]
     [:ul
      [:li "This text is automatically extracted. Please ensure that it is formatted pleasantly."]
      [:li "Remove extraneous text that is not part of the article (Eg. footer and sidebar text, unrelated links)"]
      [:li "Check that the article text is complete."]
      [:li "DO NOT edit the article text. Leave spelling errors and disagreements with content for later."]]

     [rc/input-textarea
      :model :text
      :rows 15
      :on-change (fn [text] (rf/dispatch [::set-review-text text]))]]))

(defn review-text-buttons []
  (step/stepper-buttons
   :next nil
   :buttons [[rc/button :label "Reset" :on-click #(rf/dispatch [::reset-review-text])]
             [rc/gap :size "3em"]
             [rc/button :label "Next" :on-click #(rf/dispatch [:flaglib2.stepper/goto :opine])]]))

(rf/reg-event-db
 ::set-supplied-text
 (fn [db [_ text]]
   (assoc db ::supplied-text text)))

(defn supply-text []
  [:div
   [:h3 "Supply the article text"]
   [:ul
    [:li "The article text could not be automatically downloaded and extracted."]
    [:li "You may supply the text by posting it into the box below."]
    [:li "The text should be an accurate representation of the linked article. No spelling corrections or other edits."]]
   [rc/input-textarea
    :on-change (fn [text] (rf/dispatch [::set-supplied-text text]))]])

(def steps
  [{:id :specify-target
    :label [specify-target-summary]
    :page [specify-target]}
   {:id :target-decision
    :page [target-decision]
    :buttons [target-decision-buttons]
    :label "Article text options"}
   {:id :review-text
    :page [review-text]
    :buttons [review-text-buttons]
    :once [::reset-review-text]}
   {:id :supply-text
    :previous :target-decision}
   {:id :opine}
   
            ])


;;Needs to be at bottom of file:


(defn make-opinion []
  (let [search @(rf/subscribe [::search])
        search-res @(rf/subscribe [:url-search-results])]
    [:div
     [rc/input-text
      :placeholder "Target URL or search terms"
      :model search
      :on-change (fn [ev] ev (rf/dispatch [::enter-search ev]))]
     (if search-res
       [display-searched-urls]
       [display-urls-in-categories])
     [target-decision]]))

(rf/reg-event-fx
 :make-opinion
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :target])]
     {:db (assoc db :root-element make-opinion)
      :fx [ [:dispatch
             (if target
               [::enter-search target]
               [:flaglib2.fetchers/load-author-urls])]
           [:dispatch [:add-hooks fabricate-hooks]]
           [:dispatch [:flaglib2.stepper/initialize steps]]
           ;;FIXME: is this the right place?
           [:mount-registered]]})))
