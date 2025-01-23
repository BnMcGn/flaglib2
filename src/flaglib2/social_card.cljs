(ns flaglib2.social-card
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.displayables :as disp]
   [flaglib2.titlebar :as tb]
   [flaglib2.ipfs :as ipfs]
   [flaglib2.target-summary :as tsum]))

(def card-size {:style {:max-width "1200px" :min-width "1200px"
                        :max-height "630px" :min-height "630px"
                        :background-color "white"
                        :padding "1.5rem"}})

(defn rooturl-card []
  (let [params @(rf/subscribe [:server-parameters])
        target (:target params)]
    [:div
     (merge card-size
            {:class "p-6 flex flex-col justify-around"})
     [:div
      {:style {:background-color "white" :padding "0.5em"
               :border-size "4px" :border-color "#f9f5d8"}}
      [:span {:class "text-xl bold" :style {:font-size "x-large"}}
       [tsum/display-summary-word target :class "bold text-xl"]]
      [disp/root-title
       :url target
       :hide-reply true
       :hide-count true
       :hide-external-link true
       :display-depth false
       :class "m-4"]]
     [:img {:src "/static/img/wf_logo_large.png"
            :class "absolute"
            :style {:top "390px" :left "130px" :opacity "25%"}}]
     [:div
      {:class "flex flex-row w-full justify-around self-center"
       :style {:background-color "white" :width "50rem" :justify-content "space-around"
               :align-self "center"}}
      [tsum/reply-count-long target]
      [tsum/display-other-flags target :hide-inactive true]
      [tsum/summary-scores-chart target]]]))

(defn opinion-card []
  (let [params @(rf/subscribe [:server-parameters])
        target (:target params)
        rooturl (:rooturl params)]
    [:div
     (merge card-size
            {:class "p-6 flex flex-col"})
     [disp/thread-opinion :opid target :style {:width "98%" :margin-left "0px"}]
     [:img {:src "/static/img/wf_logo_large.png"
            :class "absolute"
            :style {:top "290px" :left "300px" :opacity "25%"}}]]))

(rf/reg-event-fx
 :social-card
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :default :target])
         rooturl (get-in db [:server-parameters :default :rooturl])
         iidp (misc/iid? target)
         db (assoc db :root-element (if iidp opinion-card rooturl-card))]
     {:db db
      :fx [[:dispatch [:load-rooturl rooturl]]
           [:dispatch [:flaglib2.ipfs/request-rooturl-item rooturl "opinion-tree"]]
           [:dispatch [:mount-registered]]]
      })))
