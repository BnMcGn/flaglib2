(ns flaglib2.init
  (:require
   [goog.dom :as gdom]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [flaglib2.subscriptions]))

(rf/reg-event-fx
 ::store-server-parameters
 (fn [{:keys [db]} [_ key params]]
   {:db (assoc-in db [:server-parameters (or key :default)] params)}))

(defn mount-registered-element [db key]
  (when-let [spec (get-in db [:server-parameters key])]
    (when-let [mp (gdom/getElement (:mount-point spec))]
      (rdom/render [(or (:side-element spec) (:root-element db)) key] mp))))

(rf/reg-fx
 :mount-registered
 (fn [db & key]
   ;;Sometimes the db update doesn't arrive in time. In that case, kick to back of queue.
   (if (and key (not (= key :default)))
     (if (get-in db [:server-parameters key :side-element])
       (mount-registered-element db key)
       (rf/dispatch [:remount-registered key]))
     (if (:root-element db)
      (mount-registered-element db :default)
      (rf/dispatch [:remount-registered])))))

(rf/reg-event-fx
 :remount-registered
 (fn [{:keys [db & key]} _]
   (if key
     {:fx [ [:mount-registered db key] ]}
     {:fx (into []
                (for [k (keys (:server-parameters db))]
                  [:mount-registered db k]))})))

(defn server-side-setup [key config]
  (let [config (js->clj config :keywordize-keys true)]
    (rf/dispatch [::store-server-parameters key config])
    (rf/dispatch [(keyword (:entry-point config)) key])))



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
