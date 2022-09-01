(ns flaglib2.urlgrab
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [re-com.core :as rc]

   [cljsjs.fuse :as fuse]

   [flaglib2.misc :as misc]))


(rf/reg-sub
 :url-search-results
 (fn [db [_ location]]
   (let [search (::search (get-in db location))
         aurls (:fetchers/author-urls db)]
     (when (and search aurls)
       (let [fus (fuse/fuse (misc/reformat-urls-lists aurls)
                            (clj->js {:include-score true :keys (list :url)}))]
         (fus.search search))))))

(rf/reg-sub
 ::search
 (fn [db [_ location]]
   (::search (get-in db location))))

(rf/reg-sub
 ::selection
 (fn [db [_ location]]
   (::selection (get-in db location))))

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
              [:dispatch [disp]])
            (when (and selection onsel)
              [:call-something [onsel selection]])]))))


(defn suggest-button [location itm]
  [rc/button
   :label itm
   :on-click (fn [] (rf/dispatch [::select-url location itm]))])

(defn display-urls-in-categories [location]
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
                       [suggest-button location itm]))))]))

(defn display-searched-urls [location]
  (let [aurls @(rf/subscribe [:url-search-results location])]
    [rc/v-box
     :children
     (for [itm aurls]
       [suggest-button location (:url itm)])]))

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
          search-res @(rf/subscribe [::search-results location])]
      [:div
       [rc/input-text
        :placeholder placeholder
        :model search
        :on-change (fn [ev] (rf/dispatch [::enter-search location ev]))]
       (if search-res
         [display-searched-urls location]
         [display-urls-in-categories location])])))





