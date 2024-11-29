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

(def card-size {:width "1200px" :height "630px"})

(defn rooturl-card []
  (let [params @(rf/subscribe [:server-parameters])
        target (:target params)]
    (misc/say [:div
               (merge card-size
                      {})
               [disp/root-title :url target :hide-reply true :hide-count true :hide-external-link true]
               [tsum/summary-scores-chart target]
               [tsum/reply-count-long target]
               [tsum/display-other-flags target :hide-inactive true]
               ]))
  )

(defn opinion-card [])

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
