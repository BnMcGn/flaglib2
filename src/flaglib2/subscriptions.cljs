(ns flaglib2.subscriptions
  (:require
   [re-frame.core :as rf]
   [flaglib2.misc :as misc]

   [clojure.walk :as walk]))


(rf/reg-sub
 :server-parameters
 (fn [db _]
   (:server-parameters db)))

(rf/reg-sub
 :root-element
 (fn [db _]
   (:root-element db)))

(rf/reg-sub :flaglib2.fabricate/review-text :-> :flaglib2.fabricate/review-text)


(rf/reg-sub
 :flaglib2.fetchers/author-urls
 (fn [db _]
   (:flaglib2.fetchers/author-urls db)))


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


(rf/reg-sub
 :target-decision
 (fn [[_ target]]
   [(rf/subscribe [:warstats-store target])
    (rf/subscribe [:text-store target])
    (rf/subscribe [:text-status target])])
 (fn [[warstat text status] _]
   (let [have-text (and text (:text text))
         responses (and warstat (not (zero? (:replies-total warstat))))]
     {:status (cond (and responses have-text) :reviewed
                    have-text :available
                    ;;FIXME: Not sure that :wait is correct here. Perhaps add an appropriate message
                    ;; for the NIL circumstance
                    :else (or (walk/keywordize-keys (:status status)) :wait))
      :message (and status (:message status))})))

(rf/reg-sub
 :flaglib2.fabricate/active-text
 ;;FIXME: could consider caching a tdat?
 (fn [db _]
   (or
    (:flaglib2.fabricate/supplied-text db)
    (let [target (:flaglib2.fabricate/selection db)]
      (if target
        (get-in db [:text-store target :text])
        ;;FIXME: Should lack of text be an error? If so, what to do with it?
        "")))))

(rf/reg-sub :flaglib2.fabricate/flag :-> :flaglib2.fabricate/flag)
(rf/reg-sub
 :flaglib2.fabricate/flag-or-default
 :<- [:flaglib2.fabricate/flag]
 :<- [:server-parameters]
 (fn [[flag params] _]
   (or flag (:flag params))))

(rf/reg-sub :flaglib2.fabricate/excerpt :-> :flaglib2.fabricate/excerpt)
(rf/reg-sub
 :flaglib2.fabricate/excerpt-or-default
 :<- [:flaglib2.fabricate/excerpt]
 :<- [:server-parameters]
 (fn [[excerpt params] _]
   (or excerpt [(get params :excerpt "") (get params :offset nil)])))

(rf/reg-sub :flaglib2.fabricate/excerpt-start :-> :flaglib2.fabricate/excerpt-start)
(rf/reg-sub :flaglib2.fabricate/excerpt-search :-> :flaglib2.fabricate/excerpt-search)
