(ns flaglib2.userfig
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [clojure.string :as string]

   [re-com-tailwind.core :as rc]
   [re-com-tailwind.functions :refer [tw-btn-danger]]

   [flaglib2.misc :as misc]
   [flaglib2.deco :as deco]
   [flaglib2.fetchers :as fetch]
   [flaglib2.macros :as macros]
   [flaglib2.posters :as posters]

   ))

(macros/reg-json-fetch
 [:get-user-info
  "/userfig/get-user-info/"]
 ([result]
  (let [uinfo (misc/decapikey result)
        uinfo (update uinfo :signed-up misc/parse-time)]
    {:db (assoc (fetch/db) ::user-info uinfo)}))
 nil)

(rf/reg-sub
 ::errors
 (fn [db [_ key]]
   (if key
     (get-in db [::errors key])
     (::errors db))))

(rf/reg-sub
 ::user-info
 (fn [db [_ key]]
   (if key
     (get-in db [::user-info key])
     (::user-info db))))

(rf/reg-sub
 ::fieldspecs
 (fn [db [_ key]]
   (if key
     (get (into {} (get db ::fieldspecs)) key)
     (::fieldspecs db))))

(defn ww-simple [value dispatch & {:keys [name editable options]}]
  [rc/input-text
   :model value
   :disabled? (not editable)
   :on-change (fn [new-value]
                (reset! value new-value)
                (rf/dispatch (conj dispatch new-value)))])

(defn ww-yesno [value dispatch & {:keys [name editable options]}]
  [rc/checkbox
   :model value
   :disabled? (not editable)
   :on-change (fn [new-value]
                (reset! value new-value)
                (rf/dispatch (conj dispatch new-value)))])

(defn ww-pickone [name value dispatch])
(defn ww-picksome [name value dispatch])
(defn ww-pickone-long [name value dispatch])
(defn ww-textentry [name value dispatch])

(defn ww-date [value dispatch & {:keys [name editable options]}]
  [rc/datepicker
   :model value
   :disabled? (not editable)
   :on-change (fn [new-value]
                (reset! value new-value)
                (rf/dispatch (into dispatch new-value)))])


(def widget-map
  {:string ww-simple :integer ww-simple :boolean ww-yesno :pickone ww-pickone
   :picksome ww-picksome :pickone-long ww-pickone-long :yesno ww-yesno
   :textentry ww-textentry :date ww-date})

(rf/reg-sub ::form-items :-> ::form-items)
(rf/reg-event-db
 ::save-form-item
 (fn [db [_ key val]]
   (assoc-in db [::form-items key] val)))

(macros/reg-json-fetch
 [::save-user-info
  "/userfig/set-user-info/"
  :method :post
  :params-func
  (fn [_]
    (::form-items (fetch/db)))]
 ([result]
  {:db (assoc (fetch/db) ::status {:response (misc/kebabikey result) :failure nil})})
 ([failure]
  {:db (assoc (fetch/db) ::status {:response nil :failure (misc/kebabikey failure)})}))

(rf/reg-sub ::status :-> ::status)
(rf/reg-sub
 ::userfig-post-messages
 :<- [::status]
 (fn [status]
   (posters/msg-format status "Unable to save settings: " "Settings saved")))

(defn widget [id]
  (let [initial @(rf/subscribe [::user-info])
        model (r/atom nil)]
    (when initial
      (reset! model (get initial id))
      (fn []
        (let [fs @(rf/subscribe [::fieldspecs id])
              {:keys [widget editable options description type nullok]} fs
              err @(rf/subscribe [::errors id])
              label (or description (string/capitalize (name id)))]
          [:div {:class "mb-2"}
           [:span label
            (if-not nullok
              [:span :class "text-red-900" (misc/entities " *")])]
           [(widget-map (or widget type)) model [::save-form-item id]
            :name id
            :editable editable]
           (when err
             (deco/error-msg err))])))))

(defn simple-form [dispatch children]
  [:div {:class "border-solid border-8 rounded-lg m-2 p-2 border-blue-200"}
   children
   [rc/button
    :label "Submit"
    :on-click (fn [] (rf/dispatch dispatch))]])

(defn userfig-form []
  (let [fieldspecs @(rf/subscribe [::fieldspecs])
        initial-info @(rf/subscribe [::user-info])
        messages @(rf/subscribe [::userfig-post-messages])]
    [:div
     [simple-form
      [::save-user-info]
      (into [:<>]
            (for [[key _] fieldspecs]
              [widget key]))]
     [:div
      (into [:<>]
            (for [m messages] [:div m]))]]))

(defn normalize-fieldspec [[k v]]
  [k
   (case v
     nil false
     t true
     v)])

(defn proc-fieldspecs [fspecs]
  (loop [fspecs (cljs.reader/read-string fspecs)
         stor '()]
    (if (empty? fspecs)
      (reverse stor)
      (let [k (first fspecs)
            fspec (second fspecs)
            fields (into {} (map normalize-fieldspec (misc/pairify fspec)))
            fields (assoc fields :name k)
            splut (string/split (name k) #"::*" 2)
            k (keyword (or (second splut) (first splut)))]
        (recur (drop 2 fspecs) (conj stor [k fields]))))))

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
