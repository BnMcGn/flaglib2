(ns flaglib2.grouped
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.displayables :as disp]))

(defn direction-arrow-popup [direction iid]
  [:div
   [deco/casual-note-heading
    (case direction
      :pro "Link supports parent article"
      :con "Link contradicts parent article"
      :neutral "Link has undetermined effect on parent article")]
   [disp/opinion-info iid]])

(defn direction-arrow [& {:keys [iid]}]
  ;;FIXME: what if should be using rooturl warstat?
  (let [warstat @(rf/subscribe [:warstats-store iid])
        direction (when warstat (:direction-on-root warstat))
        imgsrc (when direction (str "/static/img/direction-" direction ".svg"))
        popup-visible? @(rf/subscribe [:flaglib2.hilited/popup-is-active? iid])]
    (when direction
      [rc/popover-anchor-wrapper
       :showing? popup-visible?
       :position :below-left
       :anchor
       [:img
        :style {:width "18px" :height "45px"}
        :src imgsrc]
       :popover
       [rc/popover-content-wrapper
        :parts {:border
                {:class "sm:w-[70rem]"
                 :style {:background-color "rgba(255, 255, 255, 0.7)"
                         :box-shadow "rgba(0, 0, 0, 0.3) 0px 0px 8px"
                         :border-radius "3px"}}}
        :arrow-renderer deco/wf-arrow
        :arrow-length 21
        :body [direction-arrow-popup direction iid]]])))

(defn display-item-rooturl [itm]
  (let [depth (:display-depth itm)] ;;FIXME: do something with...
    [:div
     [direction-arrow
      :iid (:refiid itm)]
     ;;FIXME: Need :warflagger-link?
     [disp/root-title
      :url (:url itm)
      :hide-reply true]]))

(defn display-item-reference [itm]
  (let [depth (:display-depth itm)] ;;FIXME: do something with...
    [:div
     [direction-arrow
      :iid (:refopiniid itm)]
     ;;FIXME: Need :warflagger-link?
     [disp/reference
      :url (:url itm)
      :hide-reply true]]))

(defn display-item-question [itm]
  (let [depth (:display-depth itm)] ;;FIXME: do something with...
    [:div
     [direction-arrow
      :iid (:iid itm)]
     ;;FIXME: Need :warflagger-link?
     [disp/question
      :url (:url itm)
      :hide-reply true]]))

(defn hashtags [keywords]
  (deco/casual-note-heading
   (into [:<>]
         (for [k keywords]
           [:span (str k " ")]))))

(defn display-group [group keywords]
  [:div
   (vec
    (for [itm group
          x [(when (= 0 (:display-depth itm))
               [:div [hashtags (get keywords (:url itm))]])
             ((:rowtype itm)
              :rooturl display-item-rooturl
              :reference display-item-reference
              :question display-item-question)]]
      x))])



(defn grouped-main [])

(rf/reg-event-fx
 :grouped
 (fn [{:keys [db]} _]
   {:db (assoc db :root-element grouped-main)
    :fx [ [:dispatch [:flaglib2.ipfs/request-grouped]]]}))
