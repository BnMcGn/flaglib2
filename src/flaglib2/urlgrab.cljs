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

(defn selected-url-from-db [location db]
  (::selection (get-in db location)))

(rf/reg-event-fx
 ::enter-search
 (fn [{:keys [db]} [_ location search]]
   (let [selection (when (misc/url? search) search)
         ndb (update-in
              db location
              (fn [state]
                (let [nstate (assoc state ::search search)]
                  (if selection
                    (assoc nstate ::selection selection)
                    nstate))))
         disp (when selection [:load-rooturls [selection]])
         onsel (:on-select (get-in ndb location))]
     (into {}
           [[:db ndb]
            (when disp
              [:dispatch disp])
            (when (and selection onsel)
              [:call-something [onsel selection]])]))))


(defn suggest-button [location itm]
  [rc/button
   :class (str "border-white hover:border-stone-300 " deco/button-headline-widths)
   :label [disp/root-title :url itm :hide-reply true :hide-external-link true :display-depth 0]
   :on-click (fn [] (rf/dispatch [::enter-search location itm]))])

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
                (into [:ul {:class "ml-2 list-inside"
                            :style {:list-style-image "url(\"/static/img/target-simple.svg\")"}}]
                      (for [itm items]
                        [:li [suggest-button location itm]]))]))]))

(defn display-searched-urls [location]
  (let [aurls @(rf/subscribe [:url-search-results location])]
    (when (not-empty aurls)
      (into [:ul {:class "ml-2 list-inside"
                 :style {:list-style-image "url(\"/static/img/target-simple.svg\")"}}]
           (for [itm aurls
                 :let [itm (:item itm)]]
             [:li [suggest-button location (:url itm)]])))))

(rf/reg-event-db
 ::initialize-url-search
 (fn [db [_ location on-select]]
   (assoc-in db location {:on-select on-select})))

(defn url-search [location & {:keys [on-select]}]
  (let [loc location
        onsel on-select]
    (rf/dispatch-sync [::initialize-url-search loc onsel]))
  (fn [location & {:keys [placeholder]}]
    (let [search @(rf/subscribe [::search location])
          search-res @(rf/subscribe [:url-search-results location])]
      [:div
       [rc/input-text
        :placeholder placeholder
        :model search
        :on-change (fn [ev] (rf/dispatch [::enter-search location ev]))]
       (if search-res
         [display-searched-urls location]
         [display-urls-in-categories location])])))





