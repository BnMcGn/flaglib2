(ns ^:figwheel-hooks flaglib2.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [cljs.reader]))

(println "This text is printed from src/flaglib2/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn hello-world []
  [:div
   [:h1 (:text @app-state)]
   [:h3 "Edit this in src/flaglib2/core.cljs and watch it change!"]])

(defn mount [el]
  (rdom/render [hello-world] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)


(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:warstats-store {}
    :opinion-store {}
    :text-store {}
    :title-store {}
    }))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (rf/clear-subscription-cache!)
  (mount-app-element)
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
  (mount-make-opinion nil nil))



