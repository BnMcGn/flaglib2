(ns flaglib2.userfig
  (:require
   [re-frame.core :as rf]

   [re-com-tailwind.core :as rc]
   [re-com-tailwind.functions :refer [tw-btn-danger]]

   [flaglib2.misc :as misc]
   [flaglib2.deco :as deco]
   [flaglib2.fetchers :as fetch]
   [flaglib2.macros :as macros]

   ))

(macros/reg-json-fetch
 [:get-user-info
  "/userfig/get-user-info/"]
 ([result]
  {:db (assoc (fetch/db) ::user-info result)})
 nil)

(rf/reg-sub ::user-info :-> ::user-info)
(rf/reg-sub ::fieldspecs :-> ::fieldspecs)

(defn ww-simple [name value dispatch]
  [rc/input-text
   :label name
   :model value
   :on-change (fn [new-value] (rf/dispatch (into dispatch new-value)))])

(defn ww-yesno [name value dispatch]
  [rc/checkbox
   :label name
   :model value
   :on-change (fn [new-value] (rf/dispatch (into dispatch new-value)))])

(defn userfig-form []
  (let [fieldspecs @(rf/subscribe [::fieldspecs])
        initial-info @(rf/subscribe [::user-info])]
    (println fieldspecs)
    (println initial-info)

    )
  )

(defn normalize-fieldspec [[k v]]
  [k
   (case v
     nil false
     t true
     v)])

(defn proc-fieldspecs [fspecs]
  (loop [fspecs fspecs
         stor '()]
    (if (empty? fspecs)
      (into {} stor)
      (let [k (keyword (first fspecs))
            fspec (second fspecs)
            validator (first fspec)
            fields (map normalize-fieldspec (misc/pairify fspec))
            v (into [[:validator validator]] fields)]
        (recur (drop 2 fspecs) (conj stor [k v]))))))

(rf/reg-event-fx
 :userfig-form
 (fn [{:keys [db]} _]
   (let [params (get-in db [:server-parameters :default])
         {:keys [fieldspecs data-url save-url]} params
         fieldspecs (proc-fieldspecs fieldspecs)
         db (assoc db :root-element userfig-form ::fieldspecs fieldspecs)]
     {:db db
      :fx [ [:dispatch [:get-user-info]]
            [:dispatch [:mount-registered]]]})))
