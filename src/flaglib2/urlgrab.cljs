(ns flaglib2.urlgrab
  (:require
   [re-frame.alpha :as rf]
   [re-com-tailwind.core :as rc]

   [cljsjs.fuse]

   [flaglib2.deco :as deco]
   [flaglib2.misc :as misc]
   [flaglib2.displayables :as disp]))


(rf/reg-sub
 :url-search-results
 (fn [db [_ location]]
   (let [search (::search (get-in db location))
         aurls (:flaglib2.fetchers/author-urls db)
         titles (:title-store db)]
     (when (and (not-empty search) aurls)
       (let [fus (js/Fuse. (clj->js (misc/reformat-urls-lists aurls titles))
                            (clj->js {:include-score true :keys (list :url :title)}))]
         (js->clj (. fus (search search)) :keywordize-keys true))))))

(rf/reg-sub
 ::search
 (fn [db [_ location]]
   (::search (get-in db location))))

(defn unmodified-selected-from-db [location db]
  (::selection (get-in db location)))

(defn selected-url-from-db [location db]
  (or (::modified-selection (get-in db location)) (::selection (get-in db location))))

(rf/reg-sub
 :selected-url
 (fn [db [_ location]]
   (selected-url-from-db location db)))

(rf/reg-event-db
 ::choose-adjusted-target
 (fn [db [_ location target]]
   (assoc-in db (into location [::modified-selection]) target)))

(rf/reg-event-db
 ::choose-original-target
 (fn [db [_ location]]
   (assoc-in db (into location [::modified-selection]) (unmodified-selected-from-db location db))))

(rf/reg-sub
 ::suppress-search-results
 (fn [db [_ location]]
   (:suppress-search-results (get-in db location))))

(rf/reg-event-fx
 ::enter-search
 (fn [{:keys [db]} [_ location search & {:keys [is-click?]}]]
   (let [selection (when (or (misc/iid? search) (misc/url? search)) search)
         ndb (update-in
              db location
              (fn [state]
                ;;FIXME: What if click is from a search result suggest?
                (let [nstate (assoc state ::search search :suppress-search-results is-click?)]
                  (if selection
                    (assoc nstate ::selection selection)
                    nstate))))
         disp (when selection
                (if (misc/iid? selection)
                  [:load-opinion selection]
                  [:load-rooturls [selection] :no-references true]))
         onsel (:on-select (get-in ndb location))]
     (into {}
           [[:db ndb]
            (when disp
              [:dispatch disp])
            ;;For now, we'll just trigger selection when a suggest-button is clicked.
            (when (and selection onsel)
              [:call-something [onsel selection]])]))))


(defn suggest-button [location itm]
  [rc/button
   :class "sm:border-white hover:border-stone-300 w-full sm:whitespace-nowrapper whitespace-normaller"
   :parts {:wrapper {:class "sm:w-[calc(100%_-_25px)]"}}
   :label [disp/root-title
           :url itm
           :hide-reply true
           :hide-external-link true
           :no-main-link true
           :display-depth 0]
   :on-click (fn [] (rf/dispatch [::enter-search location itm :is-click? true]))])

(defn display-urls-in-categories [location]
  (let [labels {:rooturls "Previous Targets"
                :references "Previous References"
                :replies "References from replies to your posts"}
        aurls @(rf/subscribe [:flaglib2.fetchers/author-urls])]
    [rc/v-box
     :children
     (reduce into
             (for [[cat items] aurls
                   :when (not (empty? items))]
               [(or [deco/casual-note-heading (get labels cat)] "")
                (into [:ul {:class "ml-2 sm:list-inside sm:[list-style-image:url(/static/img/target-simple.svg)]"}]
                      (for [itm items]
                        [:li [suggest-button location itm]]))]))]))

(defn display-searched-urls [location]
  (let [aurls @(rf/subscribe [:url-search-results location])]
    (when (not-empty aurls)
      (into [:ul {:class "ml-2 sm:list-inside sm:[list-style-image:url(/static/img/target-simple.svg)]"}]
           (for [itm aurls
                 :let [itm (:item itm)]]
             [:li [suggest-button location (:url itm)]])))))

(rf/reg-event-db
 :initialize-url-search
 (fn [db [_ location & {:keys [on-select]}]]
   ;;WARNING: changed to not overwriting fields already present. Might not be correct.
   (update-in db location assoc :on-select on-select :suppress-search-results false)))

(rf/reg-event-db
 ::clear-url-search
 (fn [db [_ location]]
   (update-in db location assoc ::search "" ::selection "")))

(defn url-search [location & {:keys [placeholder]}]
  (let [search @(rf/subscribe [::search location])
        search-res @(rf/subscribe [:url-search-results location])
        suppress @(rf/subscribe [::suppress-search-results location])]
    [:div
     [rc/input-text
      :placeholder placeholder
      :width "100%"
      :model search
      :on-change (fn [ev] (rf/dispatch [::enter-search location ev]))]
     (if (and (not suppress) search-res)
       [display-searched-urls location]
       [display-urls-in-categories location])]))

;;Unused...
(defn clear-button [location]
  (let [search @(rf/subscribe [::search location])
        sel @(rf/subscribe [:selected-url location])]
    (if (and (empty? search) (empty? sel))
      [rc/button
       :label "Clear"
       :class deco/button-disabled
       :disabled? true]
      [rc/button
       :label "Clear"
       :on-click #(rf/dispatch [::clear-url-search location])])))




