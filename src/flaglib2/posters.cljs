(ns flaglib2.posters
  (:require
   [re-frame.core :as rf]
   [day8.re-frame.http-fx]
   [re-com-tailwind.core :as rc]
   [ajax.core :as ajax]

   [re-com-tailwind.functions :refer [tw-btn-primary tw-btn-default-disabled]]

   [flaglib2.flags :as flags]
   [flaglib2.misc :as misc]
   [flaglib2.stepper :as step]))


(defn init []
  {::opinion-status {:response nil
                     :failure nil}
   ::alternate-status {:response nil
                     :failure nil}
   ::alt-title-status {:response nil
                     :failure nil}})

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
 (fn [_ [_ opinion opincat]]
   {:http-xhrio {:method :post
                 :uri "/opinion-post/"
                 :timeout 6000
                 :params (-> opinion
                             flag->server-style
                             remove-nulls)
                 :format (ajax/url-request-format)
                 :response-format (ajax/json-response-format)
                 :on-success [::posted-opinion-to-server opincat]
                 :on-failure [::opinion-post-failed opincat]}}))

(rf/reg-event-db
 ::posted-opinion-to-server
 (fn [db [_ opincat response]]
   (assoc-in db opincat {:response (misc/kebabikey response) :failure nil})))

(rf/reg-event-db
 ::opinion-post-failed
 ;;Response is an object that contains :success or :errors object, indexed with results from the
 ;;submission.
 (fn [db [_ opincat response]]
   (assoc-in db opincat {:response nil :failure (misc/kebabikey response)})))

;;Text/Title utilities

(defn contains-tt-tag? [text]
  (let [pattern #"^\#\((target-text|target-title|suggest-target-text|suggest-target-title)\)$"]
    (re-matches pattern text)))

(defn stick-dirc-on-text [dbody text]
  (str "#(" dbody ")\n" text))

(defn ttify-opinion [opinion tt suggest]
  (when (and (:flag opinion) (not (= :custodial-blank (:flag opinion))))
    (throw (js/Error. "Flag for text/title opinion must be :custodial-blank)")))
  (when (and (string? (:comment opinion)) (contains-tt-tag? (:comment opinion)))
    (throw (js/Error. "Already contains text/title directive")))
  (assoc opinion
         :flag :custodial-blank
         :comment (stick-dirc-on-text
                   (str (if suggest "suggest-" "") "target-" tt)
                   (or (:comment opinion) ""))))

;;Post tool for fabricate form

(defn post-status [db opincat]
  (let [{:keys [response failure]} (get-in db opincat)]
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
   (let [current-opinion (:current-opinion db)
         {:keys [alternate alt-title opinion]} current-opinion
         tg (:target opinion)
         altop (when (and (string? alternate) (not-empty alternate))
                 (ttify-opinion {:comment alternate :target tg} "text" true))
         titop (when (and (string? alt-title) (not-empty alt-title))
                 (ttify-opinion {:comment alt-title :target tg} "title" true))]
     {:fx [(when (and (not (= :posted (post-status db [::alternate-status]))) altop)
             [:dispatch [:post-opinion-to-server altop [::alternate-status]]])
           (when (and (not (= :posted (post-status db [::alt-title-status]))) titop)
             [:dispatch [:post-opinion-to-server titop [::alt-title-status]]])
           (when (not (= :posted (post-status db [::opinion-status])))
             [:dispatch [:post-opinion-to-server opinion [::opinion-status]]])]})))

(rf/reg-sub ::opinion-status :-> ::opinion-status)
(rf/reg-sub ::alternate-status :-> ::alternate-status)
(rf/reg-sub ::alt-title-status :-> ::alt-title-status)

;;FIXME: may need resets for statuses. Shouldn't be too hard.
(rf/reg-sub
 ::quick-status
 :<- [::opinion-status]
 :<- [::alternate-status]
 :<- [::alt-title-status]
 (fn [[op alt tit] _]
   [(post-status op [])
    (post-status alt [])
    (post-status tit [])]))

(defn msg-format [status text1 text2]
  (let [{:keys [response failure]} status]
   (if failure
     [(str text1 (:status-text failure))]
     (if-let [errors (:errors response)]
       (map (fn [[k v]]
              (str (if (keyword? k) (name k) k) ": " v))
            errors)
       (when (:success response)
         [text2])))))

;;FIXME: add indicator of message type. Might want to color code.
(rf/reg-sub
 :opinion-post-messages
 :<- [::opinion-status]
 :<- [::alternate-status]
 :<- [::alt-title-status]
 (fn [[op alt tit] _]
   (into []
         (flatten
          [(msg-format alt "Reviewed text failed to post: " "Reviewed text posted")
           (msg-format tit "Edited title failed to post: " "Alternate title posted")
           (msg-format op "Opinion failed to post: " "Opinion posted")]))))

(defn opine-buttons []
  (let [qstatus @(rf/subscribe [::quick-status])
        flag @(rf/subscribe [:flaglib2.fabricate/flag-or-default])
        text (if (some #(= :failed %) qstatus) "Retry" "Post")]
    [step/button-box
     (step/button-spacer
      nil
      [(if flag
         [rc/button
          :label text
          :class (tw-btn-primary)
          :on-click #(rf/dispatch [:post-opinion])]
         [rc/button
          :label text
          :class (tw-btn-default-disabled)
          :disabled? true])])]))
