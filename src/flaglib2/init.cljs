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
(defn mount-registered-elements []
  (when-let [spec @(rf/subscribe [:server-parameters])]
    (when-let [mp (gdom/getElement (:mount-point spec))]
      (rdom/render [@(rf/subscribe [:root-element])] mp))))

(rf/reg-fx
 :mount-registered
 (fn [_]
   (mount-registered-elements)))

(defn server-side-setup [config]
  (let [config (js->clj config :keywordize-keys true)]
    (rf/dispatch [::store-server-parameters config])
    (rf/dispatch [(keyword (:entry-point config))])))





