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
 (fn [db]
   ;;Sometimes the db update doesn't arrive in time. In that case, kick to back of queue.
   (if (:root-element db)
     (mount-registered-elements db)
     (rf/dispatch [:remount-registered]))))

(rf/reg-event-fx
 :remount-registered
 (fn [{:keys [db]} _]
   {:fx [ [:mount-registered db] ]}))

(defn server-side-setup [config]
  (let [config (js->clj config :keywordize-keys true)]
    (rf/dispatch [::store-server-parameters config])
    (rf/dispatch [(keyword (:entry-point config))])))



(def local-store-keys ["advanced"])

(defn do-save-to-local [storg]
  (doseq [[k v] storg]
    (.setItem js/localStorage (str k) v)))

(rf/reg-cofx
 :fetch-local-store
 (fn [cofx _]
   (assoc cofx :local-store (into {}
                                  (for [key local-store-keys]
                                    [(keyword key) (.getItem js/localStorage key)])))))

(rf/reg-event-fx
 :initialize-local-store
 [(rf/inject-cofx :fetch-local-store)]
 (fn [{:keys [db fetch-local-store]}]
   {:db (assoc db :local fetch-local-store)}))

(def save-to-local [(rf/path :local) (rf/after do-save-to-local)])
