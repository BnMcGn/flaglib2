(ns flaglib2.fetchers
  (:require
   [re-frame.core :as rf]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [flaglib2.misc :as misc]))

;;FIXME: Should check if already loaded?
(rf/reg-event-fx
 ::load-author-urls
 (fn [_ _]
   {:http-xhrio {:method :get
                 :uri "/author-url-data/"
                 :timeout 6000
                 :response-format (ajax/json-response-format)
                 :on-success
                 [::received-author-urls]
                 ;; :on-failure []
                 }}))

(rf/reg-event-db
 ::received-author-urls
 (fn [db [_ result]]
   (assoc db ::author-urls result)))

;; Text server

(rf/reg-event-fx
 :text-status
 (fn [{:keys [db]} [_ url & params]]
   (if (get (:text-status db) url)
     {}
     {:db (assoc db :text-status (assoc (:text-status db) url {}))
      :fx [ [:dispatch [::request-text-status url 10]]]})))

(rf/reg-event-fx
 ::request-text-status
 (fn [_ [_ url attempts & params]]
   {:http-xhrio {:method :get
                 :uri (str "/text-server/?url=" (js/encodeURIComponent url))
                 :timeout 6000
                 :response-format (ajax/json-response-format)
                 :on-success
                 [::received-text-status url attempts params]
                 :on-failure
                 [::failed-text-status url]
                 }}))

(rf/reg-event-fx
 ::received-text-status
 (fn [{:keys [db]} [_ url attempts {:keys [on-available]} result]]
   (let [result (walk/keywordize-keys result)
         ;;FIXME: How will json boolean act?
         available (when (:available result) on-available)]
     (if (= "wait" (:status result))
       (if (< 0 attempts)
         {:fx [[:dispatch [::request-text-status url (- attempts 1)]]]}
         {:db
          (assoc-in db [:text-status url] (assoc result :status "failure" :message "Timed out"))
          :fx [available]})
       {:db (assoc-in db [:text-status url] result)
        :fx [available]}))))

(rf/reg-event-db
 ::failed-text-status
 (fn [db [_ url result]]
   (assoc-in db [:text-status url]
             {:status "failure" :message (:last-error result) :text "" :title ""})))



