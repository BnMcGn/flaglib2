(ns ^:figwheel-hooks flaglib2.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [flaglib2.subscriptions]
   [flaglib2.init :as init]
   [flaglib2.fetchers]
   [flaglib2.ipfs :as ip]
   [flaglib2.fabricate :as fab]
   [flaglib2.posters :as posters]
   [cljs.reader]))

(println "This text is printed from src/flaglib2/core.cljs. Go ahead and edit it and see reloading in action.")

(defn window-size []
  (let [size (. js/window -innerWidth)]
    ;;Sizes taken from tailwind. Should be kept syncronized.
    (cond
      (< size 640) :sm
      (< size 768) :md
      (< size 1024) :lg
      (< size 1280) :xl
      :else :xxl)))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   (conj
    {:warstats-store {}
     :opinion-store {}
     :text-store {}
     :title-store {}
     :window-size (window-size)
     }
    (posters/init))))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (rf/clear-subscription-cache!)
  (init/mount-registered-elements)
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
                     (rf/clear-subscription-cache!)
                     (. js/window (addEventListener "resize" #(rf/dispatch [:window-size])))
                     true))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (rf/clear-subscription-cache!)
  (init/mount-registered-elements)
  ;;FIXME
  ;(mount-make-opinion nil nil)

  )

(rf/reg-event-db
 :window-size
 (fn [db _]

   (let [screen (cond
                  (< size 640) :sm
                  (< size 768) :md
                  (< size 1024) :lg
                  (< size 1280) :xl
                  :else :xxl)]
     (assoc db :window-size (window-size)))))

