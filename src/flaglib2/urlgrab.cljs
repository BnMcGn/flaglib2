(ns flaglib2.urlgrab
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
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

(rf/reg-sub
 :selected-url
 (fn [db [_ location]]
   (::selection (get-in db location))))

(rf/reg-sub
 ::suppress-search-result
 (fn [db [_ location]]
   (:suppress-search-result (get-in db location))))

(defn selected-url-from-db [location db]
  (::selection (get-in db location)))

(rf/reg-event-fx
 ::enter-search
 (fn [{:keys [db]} [_ location search & {:keys [is-click?]}]]
   (let [selection (when (misc/url? search) search)
         ndb (update-in
              db location
              (fn [state]
                ;;FIXME: What if click is from a search result suggest?
                (let [nstate (assoc state ::search search :suppress-search-result is-click?)]
                  (if selection
                    (assoc nstate ::selection selection)
                    nstate))))
         disp (when selection [:load-rooturls [selection]])
         onsel (:on-select (get-in ndb location))]
     (into {}
           [[:db ndb]
            (when disp
              [:dispatch disp])
            ;;For now, we'll just trigger selection when a suggest-button is clicked.
            (when (and selection onsel)
              [:call-something [onsel selection]])]))))


(defn suggest-button [location itm]
  (let [size @(rf/subscribe [:window-size])
        rt (if (= size :xs) disp/root-title-mobile disp/root-title)]
    [rc/button
     :class "sm:border-white hover:border-stone-300 w-full sm:whitespace-nowrapper whitespace-normaller"
    :parts {:wrapper {:class "sm:w-[calc(100%_-_25px)]"}}
    :label [rt :url itm :hide-reply true :hide-external-link true :display-depth 0]
    :on-click (fn [] (rf/dispatch [::enter-search location itm :is-click? true]))]))

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
 ::initialize-url-search
 (fn [db [_ location on-select]]
   (assoc-in db location {:on-select on-select :suppress-search-results false})))

(rf/reg-event-db
 ::clear-url-search
 (fn [db [_ location]]
   (update-in db location assoc ::search "" ::selection "")))

(defn url-search [location & {:keys [on-select]}]
  (let [loc location
        onsel on-select]
    (rf/dispatch-sync [::initialize-url-search loc onsel]))
  (fn [location & {:keys [placeholder]}]
    (let [search @(rf/subscribe [::search location])
          search-res @(rf/subscribe [:url-search-results location])
          suppress @(rf/subscribe [::suppress-search-result location])]
      [:div
       [rc/input-text
        :placeholder placeholder
        :width "100%"
        :model search
        :on-change (fn [ev] (rf/dispatch [::enter-search location ev]))]
       (if (and (not suppress) search-res)
         [display-searched-urls location]
         [display-urls-in-categories location])])))

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




