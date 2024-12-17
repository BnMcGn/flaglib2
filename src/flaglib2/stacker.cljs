(ns flaglib2.stacker
  (:require
   [re-frame.core :as rf]
   [ajax.core :as ajax]
   [cljs-time.core :as tm]
   [goog.uri.utils :as uri]
   [cljs.reader]

   [flaglib2.misc :as misc]))

;; Handles a growing stack of things from an infinite scroll source


(defn parameterize-source-url [spec]
  (let [{:keys [::stack ::limit ::url]} spec]
    (uri/setParamsFromMap url {:offset (count stack) :limit limit})))

;;FIXME: Re-request as separate module
(rf/reg-event-fx
 ::request-chunk
 (fn [{:keys [db]} [_ loc]]
   (let [spec (db loc)]
     {:http-xhrio {:method :get
                   :uri (parameterize-source-url spec)
                   :timeout (::timeout spec)
                   :response-format (::response-format spec)
                   :on-success [::received-chunk loc]
                   :on-failure [::failure loc]}
      :db (assoc-in db (into loc [::requested]) (tm/now))})))

(defn proc-chunk [spec chunk]
  (let [{:keys [::process-chunk ::process-item ::stack ::on-chunk]} spec]
    (map process-item (process-chunk chunk))))

;;FIXME: may want to hack in a cofx runner
(rf/reg-event-fx
 ::received-chunk
 (fn [{:keys [db]} [_ loc result]]
   (let [spec (get-in db loc)
         {:keys [::on-chunk ::stack]} spec
         chunk (proc-chunk spec (cljs.reader/read-string result))
         stack (into stack chunk)]
     {:db (assoc-in db loc (assoc spec ::stack stack ::requested nil))
      :fx
      (when on-chunk
        [ [:dispatch (into on-chunk chunk)]])})))

(rf/reg-event-fx
 ::failure
 (fn [{:keys [db]} [_ loc fail]]
   (let [spec (db loc)
         onerr (::on-error spec)
         newdb (assoc-in db loc (assoc spec ::requested nil))]
     (if onerr
       (if (= onerr true)
         (do
           (println "Block fetch failure: " (::url spec))
           (println "Message: " (:last-error fail))
           {:db newdb})
         {:db newdb
          :fx [ [:dispatch (into onerr fail)] ]})
       {:db newdb}))))

(rf/reg-event-fx
 ::init
 (fn [{:keys [db]} [_ loc spec]]
   (let [chunk (::chunk spec)
         spec (dissoc spec ::chunk)
         defaults
         {::timeout 18000
          ::response-format (ajax/text-response-format)
          ::process-chunk identity
          ::process-item identity
          ::on-error true
          ::on-chunk nil}
         spec (merge defaults spec)
         on-chunk (::on-chunk spec)
         chunk (when chunk (proc-chunk spec chunk))
         stack (or chunk [])]
     {:db (update-in db loc #(merge (assoc spec ::stack stack) %))
      :fx (when (and chunk on-chunk)
            [ [:dispatch (into on-chunk chunk)] ])})))

