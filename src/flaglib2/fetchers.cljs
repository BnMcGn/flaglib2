(ns flaglib2.fetchers
  (:require
   [re-frame.core :as rf]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [clojure.walk :as walk]
   [flaglib2.misc :as misc]

   [flaglib2.macros :as macros]))



(deftype RetryRequest [])

(defn retry []
  (throw (RetryRequest.)))

(def ^:dynamic *db*)

(defn db [] *db*)

(defn generic-fetcher-events-generator
  [fname
   url
   success-func
   failure-func
   &
   {:keys [timeout response-format attempts go? method]
    :or {timeout 6000 attempts nil method :get}}]
  (let [nspace (or (namespace fname) 'flaglib2.fetchers)
        req-key (keyword nspace (str "request-for-" (name fname)))
        succ-key (keyword nspace (str "success-for-" (name fname)))
        fail-key (keyword nspace (str "failure-for-" (name fname)))
        bundle {:name fname
                :url url}]

    (rf/reg-event-fx
     fname
     (fn [{:keys [db]} [_ & {:as params}]]
       (let [bundle (assoc bundle :params params)]
         (if (or (not go?) (go? db bundle))
           {:db db
            :fx [ [:dispatch [req-key bundle :attempts attempts]]]}))))

    (rf/reg-event-fx
     req-key
     (fn [_ [_ bundle & {:keys [attempts]}]]
       {:http-xhrio {:method method
                     :uri url
                     :timeout timeout
                     :params (:params bundle)
                     :format (ajax/url-request-format)
                     :response-format response-format
                     :on-success [succ-key bundle attempts]
                     :on-failure [fail-key bundle attempts]}}))

    (rf/reg-event-fx
     succ-key
     (fn [{:keys [db] :as cofx} [ename bundle attempts result]]
       (binding [*db* db]
         (try
           (success-func result (assoc bundle :db db))
           (catch RetryRequest _
             (when (and attempts (< 0 attempts))
               {:fx [[:dispatch [req-key bundle :attempts (- attempts 1)]]]}))))))

    (rf/reg-event-fx
     fail-key
     (fn [{:keys [db] :as cofx} [ename bundle attempts result]]
       (binding [*db* db]
         (try
           (failure-func result (assoc bundle :db db))
           (catch RetryRequest _
             (when (and attempts (< 0 attempts))
               {:fx [[:dispatch [req-key bundle :attempts (- attempts 1)]]]}))))))))



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
 [misc/after-hook]
 (fn [db [_ result]]
   (assoc db ::author-urls (walk/keywordize-keys result))))


;;;TEST

(macros/reg-json-fetch
 [:test-grabber
  "/text-server/"]
 ([stuff]
  (println "in success")
  (println stuff))
 nil)

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


