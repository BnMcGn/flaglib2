(ns flaglib2.subscriptions
  (:require
   [re-frame.core :as rf]
   [flaglib2.misc :as misc]
   [flaglib2.urlgrab :as urlgrab]

   [clojure.walk :as walk]))


(rf/reg-sub
 :server-parameters
 (fn [db _]
   (:server-parameters db)))

(rf/reg-sub
 :root-element
 (fn [db _]
   (:root-element db)))

(rf/reg-sub
 :flaglib2.fetchers/author-urls
 (fn [db _]
   (:flaglib2.fetchers/author-urls db)))

(rf/reg-sub
 :advanced-options
 (fn [db _]
   (get-in db [:local :advanced])))


(rf/reg-sub
 :warstats-store
 (fn [db [_ key]]
   (if key
     (get-in db [:warstats-store key])
     (:warstats-store db))))

(rf/reg-sub
 :text-store
 (fn [db [_ key]]
   (get-in db [:text-store key])))

(rf/reg-sub
 :title-store
 (fn [db [_ key]]
   (get-in db [:title-store key])))

(rf/reg-sub
 :opinion-store
 (fn [db [_ key]]
   (get-in db [:opinion-store key])))

(rf/reg-sub
 :text-status
 (fn [db [_ key]]
   (get-in db [:text-status key])))

(defn target-decision-core [[warstat text status] _]
  (let [have-text (and text (:text text))
        responses (and warstat (not (zero? (:replies-total warstat))))]
    {:status (cond (and responses have-text) :reviewed
                   have-text :available
                   ;;FIXME: Not sure that :wait is correct here. Perhaps add an appropriate message
                   ;; for the NIL circumstance
                   :else (or (keyword (walk/keywordize-keys (:status status))) :wait))
     :message (and status (:message status))}))

(defn target-decision [db target]
  (target-decision-core
   [(get-in db [:warstats-store target])
    (get-in db [:text-store target])
    (get-in db [:text-status target])]
   nil))

(rf/reg-sub
 :target-decision
 (fn [[_ target]]
   [(rf/subscribe [:warstats-store target])
    (rf/subscribe [:text-store target])
    (rf/subscribe [:text-status target])])
 target-decision-core)

(rf/reg-sub :window-size :-> :window-size)

