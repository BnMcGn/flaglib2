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
   [flaglib2.flags :as flags]
   [flaglib2.titlebar :as titlebar]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.excerpt-search :as xsearch]
   [flaglib2.typeahead :as ta]
   [flaglib2.urlgrab :as ug]
   [cljsjs.fuse :as fuse]
   [re-com.core :as rc]))

(def fabricate-hooks
  {:flaglib2.fetchers/received-author-urls [::get-stuff-for-author-urls]
   ;;FIXME: deprecated:
   ::enter-search [::get-stuff-for-selection]})

(rf/reg-event-fx
 ::get-stuff-for-author-urls
 ;;We assume that author-urls are already in ipfs. No check.
 (fn [{:keys [db]} _]
   {:dispatch [:load-rooturls
               (misc/reformat-urls-lists-simple (list (:flaglib2.fetchers/author-urls db)))
               :no-text true]}))

;;FIXME: what if user wants to start with reference, not target? way to switch?
(defn specify-target []
  [ug/url-search [::specify-target]
   :placeholder "Target URL or search terms"])

(defn specify-target-summary []
  (let [selection @(rf/subscribe [:selected-url [::specify-target]])]
    [step/summary-button :specify-target (str "Target: " selection)]))


;;Decisioner: what to do if we don't have text

(defn review-text-button []
  [rc/button
   :label "Review Text"
   :on-click (fn [] (rf/dispatch [:flaglib2.stepper/goto :review-text]))])

(defn supply-text-button []
  [rc/button
   :label "Supply Text"
   :on-click (fn [] (rf/dispatch [:flaglib2.stepper/goto :supply-text]))])


(defn target-decision []
  (let [selection @(rf/subscribe [:selected-url [::specify-target]])
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
  (let [selection @(rf/subscribe [:selected-url [::specify-target]])
        factors (and selection @(rf/subscribe [:target-decision selection]))]
    (if factors
      (case (:status factors)
        :reviewed
        (step/stepper-buttons)
        :available
        (step/stepper-buttons
         :next "Skip to Flagging"
         :buttons [[review-text-button]])
        :wait
        (step/stepper-buttons
         :next "Skip to Flagging"
         :buttons [[supply-text-button]])
        :failure
        (step/stepper-buttons
         :next "Flag Anyways"
         :buttons [[supply-text-button]]))
     [])))

(rf/reg-event-db
 ::reset-review-text
 (fn [db _]
   (let [target (ug/selected-url-from-db [::specify-target] db)]
     (assoc db ::review-text (get-in db [:text-store target :text])))))

(rf/reg-event-db
 ::set-review-text
 (fn [db [_ text]]
   (assoc db ::review-text text)))

(defn review-text []
  (let [text @(rf/subscribe [::review-text])]
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
   :buttons [[rc/button :label "Reset" :on-click #(rf/dispatch [::reset-review-text])]]))

(rf/reg-event-db
 ::set-supplied-text
 (fn [db [_ text]]
   ;;FIXME: Perhaps some processing on text?
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

(rf/reg-event-db
 ::set-flag
 (fn [db [_ flag]]
   (assoc db ::flag flag)))

(defn flag-options []
  (into []
        (for [flag flags/flag-src]
          (into {}
                (map #(apply vector %1) (partition 2 flag))))))


(defn flag-page []
  (let [flag @(rf/subscribe [::flag-or-default])]
    [:div
     [rc/single-dropdown
      :model flag
      :placeholder "Choose a Flag"
      :choices (flag-options)
      :group-fn :category
      :on-change (fn [flag] (rf/dispatch [::set-flag flag]))
      :render-fn
      (fn [item]
        [:span
         [:img {:src (titlebar/flag-icon (:id item))}]
         (str (:label item) " " (:description item))])]
     ;;FIXME: Description shouldn't show if in summary mode
     [:span (:description (get flag flags/flags))]]))

(rf/reg-event-db
 ::set-excerpt
 (fn [db [_ [excerpt offset]]]
   (assoc db ::excerpt [excerpt offset])))

(defn excerpt-page []
  (let [[excerpt offset] @(rf/subscribe [::excerpt-or-default])
        text @(rf/subscribe [::active-text])]
    [:div
     [xsearch/excerpt-search
      :text text
      :excerpt excerpt
      :offset offset
      :on-change ::set-excerpt]
     [xsearch/excerpt-search-context]]))

(defn excerpt-summary []
  (let [[excerpt _] @(rf/subscribe [::excerpt-or-default])
        text (if (or (not excerpt) (zero? (count excerpt)))
               "[no excerpt]"
               excerpt)]
    [step/summary-button :excerpt text]))



(defn specify-reference []
  [ug/url-search [::specify-reference]
   ;;FIXME: omit target URL
   :placeholder "Reference URL or search terms"])

(defn specify-reference-summary []
  (let [selection @(rf/subscribe [:selected-url [::specify-reference]])]
    [step/summary-button :reference (str "Reference: " selection)]))




(rf/reg-event-db
 ::set-comment
 (fn [db [_ comment]]
   (assoc db ::comment comment)))
(rf/reg-sub ::comment :-> ::comment)

(defn opine []
  (let [comment @(rf/subscribe [::comment])]
    [rc/input-textarea
     :model ""
     :on-change (fn [comment] (rf/dispatch [::set-comment comment]))]))

(rf/reg-event-fx
 ::opine-initialize
 (fn [{:keys [db]} _]
   {:fx
    [[:dispatch [:flaglib2.stepper/set-summary :excerpt]]
     [:dispatch [:flaglib2.stepper/set-summary :reference]]
     [:dispatch [:flaglib2.stepper/set-summary :flag]]]}))

(def steps
  [{:id :specify-target
    :label [specify-target-summary]
    :page [specify-target]}
   {:id :target-decision
    :page [target-decision]
    :buttons [target-decision-buttons]
    :label "Article text options"
    :next :opine}
   {:id :review-text
    :page [review-text]
    :buttons [review-text-buttons]
    :once [::reset-review-text]
    :next :opine}
   {:id :supply-text
    :previous :target-decision}
   {:id :flag
    :page [flag-page]
    :label [flag-page]
    ;:buttons ""
    }
   {:id :excerpt
    :page [excerpt-page]
    :label [excerpt-summary]
    :buttons [xsearch/excerpt-search-buttons]
    }
   {:id :reference
    :page [specify-reference]
    :label [specify-reference-summary]}
   {:id :opine
    :label [opine]
    :page [opine]
    :once [::opine-initialize]
    }])


;;Needs to be at bottom of file:

(defn make-opinion []
  (let [search @(rf/subscribe [:flaglib2.urlgrab/search [::specify-target]])
        search-res @(rf/subscribe [:url-search-results [::specify-target]])]
    [step/wf-stepper]))

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
