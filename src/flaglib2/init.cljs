(ns flaglib2.init
  (:require
   [goog.dom :as gdom]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [flaglib2.subscriptions]))

(rf/reg-event-fx
 ::store-server-parameters
 (fn [{:keys [db]} [_ params]]
   {:db (assoc db :server-parameters params)}))


;;FIXME: Need to handle multiple?
(defn mount-registered-elements [db]
  (when-let [spec (:server-parameters db)]
    (when-let [mp (gdom/getElement (:mount-point spec))]
      (rdom/render [(:root-element db)] mp))))

(rf/reg-fx
 :mount-registered
 (fn [_ db]
   (mount-registered-elements db)))

(rf/reg-event-fx
 :remount-registered
 (fn [{:keys [db]} _]
   {:fx [ [:mount-registered db] ]}))

(defn server-side-setup [config]
  (let [config (js->clj config :keywordize-keys true)]
    (rf/dispatch [::store-server-parameters config])
    (rf/dispatch [(keyword (:entry-point config))])))





