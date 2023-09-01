(ns flaglib2.ipfs
  (:require
   [goog.dom :as gdom]
   [re-frame.core :as rf]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [clojure.string :as string]
   [clojure.edn :as edn]
   [flaglib2.misc :as misc]
   [flaglib2.fetchers :as fetchers]
   [cljs-time.core :as time]))


(defn opinion-data-url [iid itype]
  (str "/ipns/" window.IPNSHOST "/opinions/" iid "/" itype ".data"))

(defn rooturl-data-url [rooturl rtype]
  (str "/ipns/" window.IPNSHOST "/rooturls/"
       (.replaceAll (misc/encode-uri-component2 rooturl) "%" "*")
       "/" rtype ".data"))

(rf/reg-event-fx
 ::request-rooturl-item
 (fn [{:keys [db]} [_ rooturl resource-type]]
   ;;FIXME: could add indicator to db that request is pending...
   {:http-xhrio {:method :get
                 :uri (rooturl-data-url rooturl resource-type)
                 :timeout 18000
                 :response-format (ajax/text-response-format)
                 :on-success
                 [(keyword 'flaglib2.ipfs (string/join ["received-" resource-type]))
                  rooturl]
                 :on-failure [::failure rooturl resource-type]
                 }}))

(rf/reg-event-fx
 ::request-opinion-item
 (fn [{:keys [db]} [_ iid resource-type]]
   ;;FIXME: could add indicator to db that request is pending...
   {:http-xhrio {:method :get
                 :uri (opinion-data-url iid resource-type)
                 :timeout 18000
                 :response-format (ajax/text-response-format)
                 :on-success
                 [(keyword 'flaglib2.ipfs (string/join ["received-" resource-type]))
                  iid]
                 :on-failure [::failure iid resource-type]
                 }}))

(defn proc-warstat [data]
  (let [{:as warstat} (cljs.reader/read-string data)]
    (if (:tree-freshness warstat)
      (assoc
       warstat
       :tree-freshness (misc/parse-time (:tree-freshness warstat)))
      warstat)))

(rf/reg-event-fx
 ::received-warstats
 (fn [{:keys [db]} [_ key result]]
   {:db (assoc db ::warstats-tmp (assoc (::warstats-tmp db) key (proc-warstat result)))
    :fx [ [:dispatch [::start-debounce]] ]}))

(defn proc-text [data]
  (let [{:as text} (cljs.reader/read-string data)]
   text))

(rf/reg-event-fx
 ::received-text
 (fn [{:keys [db]} [_ key result]]
   {:db (assoc db ::text-tmp (assoc (::text-tmp db) key (proc-text result)))
    :fx [ [:dispatch [::start-debounce]] ]}))

(defn proc-title [data]
  (let [{:as title} (cljs.reader/read-string data)]
    title))

(rf/reg-event-fx
 ::received-title
 (fn [{:keys [db]} [_ key result]]
   {:db (assoc db ::title-tmp (assoc (::title-tmp db) key (proc-title result)))
    :fx [ [:dispatch [::start-debounce]] ]}))

(defn proc-opinion [data]
  (let [{:as opinion} (cljs.reader/read-string data)]
    (assoc
     opinion
     :created (misc/parse-time (:created opinion))
     :flag (let [[a b] (:flag opinion)]
             (keyword (str (name a) "-" (name b)))))))

(rf/reg-event-fx
 ::received-opinion
 (fn [{:keys [db]} [_ key result]]
   {:db (assoc db ::opinion-tmp (assoc (::opinion-tmp db) key (proc-opinion result)))
    :fx [ [:dispatch [::start-debounce]] ]}))

(rf/reg-event-fx
 ::received-references
 (fn [{:keys [db]} [_ key result]]
   (let [{:keys [references refd]} (into {} (map vec (partition 2 (cljs.reader/read-string result))))
         iids (into (filter misc/iid? references) (filter misc/iid? refd))
         rooturls (remove misc/iid? references)
         dispatches (map #(vector :dispatch [:load-opinion %]) iids)
         dispatches (into dispatches (when rooturls
                                       [:dispatch [:load-rooturls rooturls
                                                   :no-text true
                                                   :no-references true]]))]
     {:db (-> db
              (assoc-in [:references key] references)
              (assoc-in [:refd key] refd))
      ;;FIXME: Might want a way to disable auto load
      ;;NOTE: Might switch to warpaks...
      :fx (into [] dispatches)})))

(rf/reg-event-fx
 ::received-opinion-tree
 (fn [{:keys [db]} [_ key result]]
   (let [result (cljs.reader/read-string result)]
     {:db (assoc-in db [:opinion-tree-store key] result)
      :fx [ [:dispatch [:load-opinions (flatten result)]]]})))

(rf/reg-event-db
 ::failure
 (fn [db [_ iid type spec]]
   (println "IPFS fetch failure: " iid " Type:" type (:last-error spec))
   db))

(rf/reg-event-fx
 ::start-debounce
 (fn [{:keys [db]} _]
   (if (::debouncing db)
     {}
     {:db (assoc db ::debouncing true)
      :fx [ [:dispatch-later {:ms 500 :dispatch [::complete-debounce]}] ]})))

(rf/reg-event-db
 ::complete-debounce
 (fn [db _]
   (assoc
    db
    :warstats-store (merge (:warstats-store db) (::warstats-tmp db))
    ::warstats-tmp {}
    :text-store (merge (:text-store db) (::text-tmp db))
    ::text-tmp {}
    :title-store (merge (:title-store db) (::title-tmp db))
    ::title-tmp {}
    :opinion-store (merge (:opinion-store db) (::opinion-tmp db))
    ::opinion-tmp {}
    ::debouncing nil)))

(rf/reg-event-fx
 :load-rooturl
 (fn [_ [_ rooturl & {:keys [no-text no-references]}]]
   {:fx [[:dispatch [:flaglib2.ipfs/request-rooturl-item rooturl "warstats"]]
         [:dispatch [:flaglib2.ipfs/request-rooturl-item rooturl "title"]]
         (when-not no-references
           [:dispatch [:flaglib2.ipfs/request-rooturl-item rooturl "references"]])
         (when-not no-text
           [:dispatch [:flaglib2.ipfs/request-rooturl-item rooturl "text"]])]}))

(rf/reg-event-fx
 :load-opinion
 (fn [_ [_ iid]]
   {:fx [[:dispatch [:flaglib2.ipfs/request-opinion-item iid "warstats"]]
         [:dispatch [:flaglib2.ipfs/request-opinion-item iid "title"]]
         [:dispatch [:flaglib2.ipfs/request-opinion-item iid "opinion"]]]}))


;; Warstats requester
;; High level multiple requester with safety features

;;FIXME: gently seems broken
(rf/reg-event-fx
 :load-rooturls
 (fn [_ [_ rooturls & {:keys [no-text gently no-references]}]]
   {:fx
    (into []
          (for [url rooturls]
            (let [ev [:load-rooturl url :no-text no-text :no-references no-references]]
              (if gently
                [:dispatch [:text-status url :on-available ev]]
                [:dispatch ev]))))}))

(rf/reg-event-fx
 :load-opinions
 (fn [_ [_ opids]]
   {:fx
    (into []
          (for [iid opids]
            [:dispatch [:load-opinion iid]]))}))
