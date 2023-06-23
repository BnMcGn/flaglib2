(ns flaglib2.forms
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]

   [flaglib2.fetchers :as fetchers]
   [flaglib2.ipfs :as ip]
   [flaglib2.misc :as misc]
   [flaglib2.subscriptions :as subs]
   [flaglib2.stepper :as step]
   [flaglib2.flags :as flags]
   [flaglib2.titlebar :as titlebar]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.excerpt-search :as xsearch]
   [flaglib2.typeahead :as ta]
   [flaglib2.urlgrab :as ug]
   [flaglib2.posters :as posters]

   [cljsjs.fuse :as fuse]
   [re-com-tailwind.core :as rc]
   [re-com-tailwind.functions :refer [tw-btn-danger]]))

(def fabricate-hooks
  {:flaglib2.fetchers/received-author-urls [:flaglib2.fabricate/get-stuff-for-author-urls]})


;;FIXME: what if user wants to start with reference, not target? way to switch?
(defn specify-target []
  [ug/url-search [:flaglib2.fabricate/specify-target]
   :placeholder "Target URL or search terms"])

(defn specify-target-summary []
  (let [selection @(rf/subscribe [:selected-url [:flaglib2.fabricate/specify-target]])]
    [step/summary-button :specify-target (str "Target: " selection)]))


(defn specify-target-buttons []
  (let [url @(rf/subscribe [:selected-url [:flaglib2.fabricate/specify-target]])]
    [step/button-box
     (step/button-spacer
      [[step/previous-button nil]]
      [(if (empty? url) [step/next-button-disabled] [step/next-button nil])])]))

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
  (let [selection @(rf/subscribe [:selected-url [:flaglib2.fabricate/specify-target]])
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
          [:li "You might not need the article text in some cases, for example, if you aren't using excerpts."]]])
      "")))



(defn target-decision-buttons []
  (let [selection @(rf/subscribe [:selected-url [:flaglib2.fabricate/specify-target]])
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

;;FIXME: Should also review title?
(defn review-text []
  (let [text @(rf/subscribe [:flaglib2.fabricate/review-text])]
    [:div
     [:h3 "Review article text for tidyness"]
     [:ul
      [:li "This text is automatically extracted. Please ensure that it is formatted pleasantly."]
      [:li "Remove extraneous text that is not part of the article (Eg. footer and sidebar text, unrelated links)"]
      [:li "Check that the article text is complete."]
      [:li "Ensure that the title is uncluttered and informative."]
      [:li "DO NOT edit the article text. Leave spelling errors and disagreements with content for later."]
      [:br]
      [:li "To skip, click Next"]]
     [:br]

     [rc/input-text
      :model (rf/subscribe [:flaglib2.fabricate/supplied-title])
      :placeholder "Article Title"
      :on-change (fn [title] (rf/dispatch [:flaglib2.fabricate/set-supplied-title title]))]
     [rc/input-textarea
      :model (rf/subscribe [:flaglib2.fabricate/supplied-text])
      :placeholder "Article Text"
      :width "100%"
      :height "14rem"
      :rows 15
      :on-change (fn [text] (rf/dispatch [:flaglib2.fabricate/set-supplied-text text]))]]))

;;FIXME: Previous button non-functional
(defn review-text-buttons []
  (step/stepper-buttons
   :buttons [[rc/button :label "Reset" :on-click #(rf/dispatch [:flaglib2.fabricate/reset-supplied-tt])]]))

(defn supply-text []
  [:div
   [:h3 "Supply the article text and title"]
   [:ul
    [:li "The article text could not be automatically downloaded and extracted."]
    [:li "You may supply the text and title by posting into the boxes below."]
    [:br]
    [:li "The text should be an accurate representation of the linked article. No spelling corrections or other edits."]]
   [:br]

   [rc/input-text
    :model (rf/subscribe [:flaglib2.fabricate/supplied-title])
    :placeholder "Article Title"
    :on-change (fn [title] (rf/dispatch [:flaglib2.fabricate/set-supplied-title title]))]
   [rc/input-textarea
    :model (rf/subscribe [:flaglib2.fabricate/supplied-text])
    :placeholder "Article Text"
    :width "100%"
    :height "14rem"
    :rows 15
    :on-change (fn [text] (rf/dispatch [:flaglib2.fabricate/set-supplied-text text]))]])

(defn flag-options []
  (into []
        (for [flag flags/flag-src]
          (into {}
                (map #(apply vector %1) (partition 2 flag))))))


(defn flag-page []
  (let [flag @(rf/subscribe [:flaglib2.fabricate/flag-or-default])
        finfo (when flag (get flags/flags flag))]
    [:div
     {:class "w-full"}
     [rc/single-dropdown
      :model flag
      :placeholder "Choose a Flag"
      :width "90%"
      :choices (flag-options)
      :group-fn :category
      :on-change (fn [flag] (rf/dispatch [:flaglib2.fabricate/set-flag flag]))
      :label-fn (fn [item]
                  [:span (str (:category item) ": " (:label item))])
      :render-fn (fn [item]
                   [rc/h-box
                    :align :center
                    :children
                    [[:img {:src (titlebar/flag-icon (:id item))}]
                     [:span (str (:category item) ": " (:label item))]]])]
     (if flag
       [rc/info-button
        :info [:div
               [:div
                {:class "flex items-center mb-2"}
                [:img {:src (titlebar/flag-icon (:id finfo))}]
                [:span {:class "ml-3"} (str (:category finfo) ": " (:label finfo))]]
               (:description finfo)]]
       [rc/md-circle-icon-button
        :md-icon-name "zmdi-alert-triangle"
        :size :smaller
        :tooltip "Flag is mandatory"
        :class (misc/class-string (tw-btn-danger))]
         )]))

(defn excerpt-page []
  (let [[excerpt offset] @(rf/subscribe [:flaglib2.fabricate/excerpt-or-default])
        text @(rf/subscribe [:flaglib2.fabricate/active-text])]
    [:div
     [xsearch/excerpt-search
      :text text
      :excerpt excerpt
      :offset offset
      :on-change :flaglib2.fabricate/set-excerpt]
     [xsearch/excerpt-search-context]]))

;;FIXME: should be a visual indication for missing excerpt
(defn excerpt-summary []
  (let [[excerpt _] @(rf/subscribe [:flaglib2.fabricate/excerpt-or-default])
        text (if (empty? excerpt)
               "Choose an Excerpt"
               excerpt)]
    [step/summary-button :excerpt text]))

(defn specify-reference []
  [ug/url-search [::specify-reference]
   ;;FIXME: omit target URL
   :placeholder "Reference URL or search terms"])

(defn specify-reference-summary []
  (let [selection @(rf/subscribe [:selected-url [:flaglib2.fabricate/specify-reference]])
        text (if (empty? selection) "Set a Reference" (str "Reference: " selection))]
    [step/summary-button :reference text]))

(defn reference-buttons []
  [step/button-box (step/button-spacer nil [[step/next-button "Accept"]])])

(defn opine []
  (let [comment @(rf/subscribe [:flaglib2.fabricate/comment])
        messages @(rf/subscribe [:opinion-post-messages])]
    [:div
     [rc/input-textarea
      :model ""
      :width "100%"
      :height "14rem"
      :placeholder "Enter a Comment"
      :on-change (fn [comment] (rf/dispatch [:flaglib2.fabricate/set-comment comment]))]
     [rc/v-box
      :children
      (into []
            (for [m messages]
              [:div m]))]]))

(rf/reg-event-fx
 ::opine-initialize
 (fn [{:keys [db]} _]
   {:fx
    [[:dispatch [:flaglib2.stepper/set-summary :excerpt]]
     [:dispatch [:flaglib2.stepper/set-summary :reference]]
     [:dispatch [:flaglib2.stepper/set-summary :flag]]]}))

(def steps-advanced
  [{:id :specify-target
    :label [specify-target-summary]
    :page [specify-target]
    :buttons [specify-target-buttons]
    :next (fn [db]
            (if (= :reviewed (:status
                              (subs/target-decision
                               db
                               (ug/selected-url-from-db [:flaglib2.fabricate/specify-target] db))))
              :opine
              :target-decision))}
   {:id :target-decision
    :page [target-decision]
    :buttons [target-decision-buttons]
    :label "Article text options"
    :next :opine}
   {:id :review-text
    :page [review-text]
    :buttons [review-text-buttons]
    :once [:flaglib2.fabricate/reset-supplied-tt]
    :next :opine}
   {:id :supply-text
    :page [supply-text]
    :previous :target-decision
    :next :opine}
   {:id :flag
    :page [flag-page]
    :label [flag-page]
    :grouped true
    ;:buttons ""
    }
   {:id :excerpt
    :page [excerpt-page]
    :label [excerpt-summary]
    :grouped true
    :buttons [xsearch/excerpt-search-buttons]
    }
   {:id :reference
    :page [specify-reference]
    :grouped true
    :buttons [reference-buttons]
    :label [specify-reference-summary]}
   {:id :opine
    :label [opine]
    :grouped true
    :page [opine]
    :buttons [posters/opine-buttons]
    :once [::opine-initialize]
    }])

;;Needs to be at bottom of file:

(defn make-opinion []
  (let [search @(rf/subscribe [:flaglib2.urlgrab/search [:flaglib2.fabricate/specify-target]])
        search-res @(rf/subscribe [:url-search-results [:flaglib2.fabricate/specify-target]])]
    [step/wf-stepper]))

(rf/reg-event-fx
 :make-opinion
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :target])
         db (assoc db :root-element make-opinion)]
     {:db db
      :fx [ [:dispatch
             (if target
               [:flaglib2.fabricate/enter-search target]
               [:flaglib2.fetchers/load-author-urls])]
           [:dispatch [:add-hooks fabricate-hooks]]
           [:dispatch [:flaglib2.stepper/initialize steps-advanced]]
           ;;FIXME: is this the right place?
           [:mount-registered db]]})))
