(ns flaglib2.posters
  (:require
   [re-frame.core :as rf]
   [day8.re-frame.http-fx]
   [re-com.core :as rc]
   [ajax.core :as ajax]
   [clojure.string :as string]
   [clojure.walk :as walk]

   [flaglib2.flags :as flags]
   [flaglib2.misc :as misc]))


(defn init []
  {::opinion-response nil
   ::opinion-failure nil
   ::alternate-response nil
   ::alternate-failure nil})


;;Low level

(defn flag->server-style [opinion]
  (let [flag ((:flag opinion) flags/flags)]
    (if flag
      (assoc opinion :flag (str (:category flag) ": " (:label flag)))
      opinion)))

(defn remove-nulls [thing]
  (into {}
        (for [[k v] thing
              :when v]
          [k v])))

(rf/reg-event-fx
 :post-opinion-to-server
 (fn [_ [_ opinion callback & {:keys [failure]}]]
   {:http-xhrio {:method :post
                 :uri "/opinion-post/"
                 :timeout 6000
                 :params (-> opinion
                             flag->server-style
                             remove-nulls)
                 :format (ajax/url-request-format)
                 :response-format (ajax/json-response-format)
                 :on-success [::posted-opinion-to-server callback]
                 :on-failure [::opinion-post-failed failure]}}))

(rf/reg-event-fx
 ::posted-opinion-to-server
 (fn [_ [_ callback response]]
   {:call-something (conj callback (walk/keywordize-keys response))}))

(rf/reg-event-fx
 ::opinion-post-failed
 (fn [_ [_ callback response]]
   (if callback
     {:call-something (conj callback response)}
     {})))


;;Text/Title utilities

(defn contains-tt-tag? [text]
  (let [pattern #"^\#\((target-text|target-title|suggest-target-text|suggest-target-title)\)$"]
    (re-matches pattern text)))

(defn stick-dirc-on-text [dbody text]
  (str "#(" dbody ")\n" text))

(defn ttify-opinion [opinion tt suggest]
  (when (and (:flag opinion) (not (= '(:custodial :blank) (:flag opinion))))
    (throw (js/Error. "Flag for text/title opinion must be (:custodial :blank)")))
  (when (and (string? (:comment opinion)) (contains-tt-tag? (:comment opinion)))
    (throw (js/Error. "Already contains text/title directive")))
  (assoc opinion
         :flag '(:custodial :blank)
         :comment (stick-dirc-on-text
                   (str (if suggest "suggest-" "") "target-" tt)
                   (or (:comment opinion) ""))))

;;Post tool for fabricate form

(defn opinion-post-status [db]
  (let [response (::opinion-response db)
        failure (::opinion-failure db)]
    (cond
      failure :failed
      response
      (cond
        (:errors response) :failed
        (:success response) :posted
        :else (throw (js/Error. "Invalid status from server"))) ;;Shouldn't happen
      :else :unposted)))

(defn alternate-post-status [db]
  (let [response (::alternate-response db)
        failure (::alternate-failure db)]
    (cond
      failure :failed
      response
      (cond
        (:errors response) :failed
        (:success response) :posted
        :else (throw (js/Error. "Invalid status from server"))) ;;Shouldn't happen
      :else :unposted)))


;;FIXME: should check for previous failed before posting both opin and alt.
(rf/reg-event-fx
 :post-opinion
 (fn [{:keys [db]} _]
   (let [{:keys [opinion alternate]} @(rf/subscribe [:current-opinion])
         altop (when (and (string? alternate) (not-empty alternate))
                 (ttify-opinion {:comment alternate} "text" false))]
     {:fx [(when (and (not (= :posted (alternate-post-status db))) altop)
             [:dispatch [:post-opinion-to-server
                         altop [::alternate-response] :failure [::alternate-failure]]])
           (when (not (= :posted (opinion-post-status db)))
             [:dispatch [:post-opinion-to-server opinion [::opinion-response]
                         :failure [::opinion-failure]]])]})))

(rf/reg-event-db
 ::opinion-response
 (fn [db [_ response]]
   (assoc db ::opinion-response response ::opinion-failure nil)))

(rf/reg-event-db
 ::opinion-failure
 (fn [db [_ response]]
   (assoc db ::opinion-failure response ::opinion-response nil)))

(rf/reg-event-db
 ::alternate-response
 (fn [db [_ response]]
   ;;Response is an object that contains :success or :errors object, indexed with results from the
   ;;submission.
   (assoc db ::alternate-response response ::alternate-failure nil)))

(rf/reg-event-db
 ::alternate-failure
 (fn [db [_ response]]
   (assoc db ::alternate-failure response ::alternate-response nil)))

(rf/reg-sub ::opinion-failure :-> ::opinion-failure)
(rf/reg-sub ::opinion-response :-> ::opinion-response)
(rf/reg-sub ::alternate-failure :-> ::alternate-failure)
(rf/reg-sub ::alternate-response :-> ::alternate-response)

;;FIXME: may need resets for statuses. Shouldn't be too hard.
(rf/reg-sub
 :opinion-post-status
 :<- [::opinion-response]
 :<- [::opinion-failure]
 (fn [[response failure] _]
   (opinion-post-status {::opinion-response response ::opinion-failure failure})))

(rf/reg-sub
 :alternate-post-status
 :<- [::alternate-response]
 :<- [::alternate-failure]
 (fn [[response failure] _]
   (alternate-post-status {::alternate-response response ::alternate-failure failure})))

;;FIXME: add indicator of message type.
(rf/reg-sub
 :opinion-post-messages
 :<- [::opinion-response]
 :<- [::opinion-failure]
 :<- [::alternate-response]
 :<- [::alternate-failure]
 (fn [[oresp ofail aresp afail] _]
   (into []
         (flatten
          [(if afail
             [(str "Reviewed text failed to post: " (:status-text afail))]
             (if-let [errors (:errors aresp)]
               (map (fn [[k v]] (str k ": " v)) errors)
               (when (:success aresp)
                 ["Reviewed text posted"])))
           (if ofail
             [(str "Opinion failed to post: " (:status-text ofail))]
             (if-let [errors (:errors oresp)]
               (map (fn [[k v]] (str k ": " v)) errors)
               (when (:success oresp)
                 ["Opinion posted"])))]))))

(defn opine-buttons []
  (let [ostatus @(rf/subscribe [:opinion-post-status])
        astatus @(rf/subscribe [:alternate-post-status])]
    (rc/h-box
     :children
     (if (some #(= :failed %) [ostatus astatus])
       [[rc/button :label "Retry" :on-click #(rf/dispatch [:post-opinion])]]
       [[rc/button :label "Post" :on-click #(rf/dispatch [:post-opinion])]]))))
