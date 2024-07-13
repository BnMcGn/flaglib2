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
   [flaglib2.forms :as forms]
   [flaglib2.target]
   [flaglib2.opinion-page]
   [flaglib2.posters :as posters]
   [flaglib2.grouped :as grouped]
   [flaglib2.mock-make :as mock]
   [flaglib2.things :as things]
   [cljs.reader]))


(defn window-size []
  (let [size (. js/window -innerWidth)]
    ;;Sizes taken from tailwind. Should be kept syncronized.
    (cond
      (< size 640) :xs
      (< size 768) :sm
      (< size 1024) :md
      (< size 1280) :lg
      (< size 1536) :xl
      :else :xxl)))


(rf/reg-event-fx
 :initialize
 (fn [_ _]
   {:db
    (conj
     {:warstats-store {}
      :opinion-store {}
      :text-store {}
      :title-store {}
      :window-size (window-size)
      }
     (posters/init))
    :fx [[:dispatch [:initialize-local-store]]]}))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (rf/clear-subscription-cache!)
  (rf/dispatch-sync [:remount-registered])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

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
  (rf/dispatch-sync [:remount-registered]))

(rf/reg-event-db
 :window-size
 (fn [db _]
   (assoc db :window-size (window-size))))

