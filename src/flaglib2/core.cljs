(ns ^:figwheel-hooks flaglib2.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [flaglib2.ipfs :as ip]
   [flaglib2.fabricate :as fab]
   [cljs.reader]))

(println "This text is printed from src/flaglib2/core.cljs. Go ahead and edit it and see reloading in action.")

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:warstats-store {}
    :opinion-store {}
    :text-store {}
    :title-store {}
    }))

(rf/reg-event-fx
 ::store-server-parameters
 (fn [{:keys [db]} [_ params]]
   {:db (assoc db :server-parameters params)}))

(rf/reg-sub
 :server-parameters
 (fn [db _]
   (:server-parameters db)))

(rf/reg-sub
 :root-element
 (fn [db _]
   (:root-element)))

;;FIXME: Need to handle multiple?
(defn mount-registered-elements []
  (when-let [spec @(rf/subscribe [:server-parameters])]
    (when-let [mp (js/document.getElementById (:mount-point spec))]
      (rdom/render [@(rf/subscribe :root-element)] mp))))

(rf/reg-fx
 :mount-registered
 (fn [_]
   (mount-registered-elements)))

(defn server-side-setup [config]
  (let [config (js->clj config :keywordize-keys true)]
    (rf/dispatch [::store-server-parameters config])
    (rf/dispatch [(keyword (:entry-point config))])))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (rf/clear-subscription-cache!)
  (mount-registered-elements)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn make-opinion []
  [:span "here!"])

;;FIXME: temporary hack. Need a better way to store mount points.
(defonce mount-point nil)
;;FIXME: will go somewhere else
(defn mount-make-opinion [element params]
  (when element
    (set! mount-point element))
  ;;params, when available, should be sent to re-frame.

  (rdom/render [make-opinion] mount-point))

(defonce startup (do (rf/dispatch-sync [:initialize])
                     true))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (rf/clear-subscription-cache!)
  ;;FIXME
  ;(mount-make-opinion nil nil)

  )






