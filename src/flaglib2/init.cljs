(ns ^:export flaglib2.init
  (:require
   [goog.dom :as gdom]
   [goog.object :as go]
   [reagent.dom :as rdom]
   [re-frame.alpha :as rf]
   [flaglib2.subscriptions]
   [flaglib2.userfig :as userfig]

   [day8.re-frame-10x :as tenx]))

(rf/reg-event-fx
 ::store-server-parameters
 (fn [{:keys [db]} [_ key params]]
   {:db (assoc-in db [:server-parameters (or key :default)] params)}))


(rf/reg-fx
 :do-mount
 (fn [{:keys [mount-point element key]}]
   (when-let [mp (gdom/getElement mount-point)]
     (rdom/render [element key] mp))))

(rf/reg-event-fx
 :mount-registered
 (fn [{:keys [db]} [_ key]]
   (let [sp (:server-parameters db)
         key (or key :default)
         spec (and sp (get sp key))
         mp (:mount-point spec)
         elt (or (:side-element spec) (:root-element db))]
     (if (and mp elt)
       {:do-mount {:mount-point mp :element elt :key key}}
       {}))))

(rf/reg-event-fx
 :remount-registered
 (fn [{:keys [db]} [_ key]]
   (if key
     {:fx [ [:dispatch [:mount-registered key]] ]}
     {:fx (into []
                (for [k (keys (:server-parameters db))]
                  [:dispatch [:mount-registered {:db db :key (or k :default)}]]))})))

(defn ^:export server-side-setup [key config]
  (let [config (js->clj config :keywordize-keys true)]
    (rf/dispatch [::store-server-parameters key config])
    (rf/dispatch [:userfig/store-user-info (js->clj (go/get js/window "USERFIGDATA"))])
    (rf/dispatch [(keyword (:entry-point config)) key])))

(def local-store-keys ["advanced" "warn-off-overrides"])

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

(defn start-10x []
  (tenx/patch!)
  (rf/dispatch-sync [:day8.re-frame-10x.events/init tenx/project-config])
  ;(rf/clear-subscription-cache!)
  (def sroot (tenx/create-shadow-root nil))
  (rdom/render (tenx/create-style-container sroot) sroot))

