(ns flaglib2.ipfs
  (:require
   [goog.object :as go]
   [re-frame.core :as rf]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [clojure.string :as string]
   [clojure.set]
   [cljs.reader]

   [flaglib2.misc :as misc]))


(defn data-url [host rest]
  (str "/ipns/" host "/" rest))

(defn opinion-data-url [host iid itype]
  (str "/ipns/" host "/opinions/" iid "/" itype ".data"))

(defn rooturl-data-url [host rooturl rtype]
  (str "/ipns/" host "/rooturls/"
       (.replaceAll (misc/encode-uri-component2 rooturl) "%" "*")
       "/" rtype ".data"))

(rf/reg-event-fx
 ::request-rooturl-item
 (fn [{:keys [db]} [_ rooturl resource-type]]
   ;;FIXME: could add indicator to db that request is pending...
   {:http-xhrio {:method :get
                 :uri (rooturl-data-url (:ipns-host db) rooturl resource-type)
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
                 :uri (opinion-data-url (:ipns-host db) iid resource-type)
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
 [misc/after-hook]
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

;;Always returns the whole warstats-tmp db, change or no
(defn proc-refd-warstat [opinion db]
  (if-let [refd (:refd-opinion opinion)]
    (update-in
     (::warstats-tmp db) [refd :refd]
     #(conj (or %1 #{}) (:iid opinion)))
    (::warstats-tmp db)))

(rf/reg-event-fx
 ::received-opinion
 (fn [{:keys [db]} [_ key result]]
   (let [opinion (proc-opinion result)
         warstats (proc-refd-warstat opinion db)
         ref-opin (:refd-opinion opinion)
         ;;FIXME: Slight possibility of endless opinion chain
         ref-opin-loader (if ref-opin
                           [:dispatch [:load-opinion ref-opin]]
                           [])]
     {:db (assoc db ::opinion-tmp (assoc (::opinion-tmp db) key opinion)
                 ::warstats-tmp warstats)
      :fx [ (into ref-opin-loader [:dispatch [::start-debounce]]) ]})))

(rf/reg-event-fx
 ::received-references
 (fn [{:keys [db]} [_ key result]]
   (let [{:keys [references refd]} (into {} (map vec (partition 2 (cljs.reader/read-string result))))
         iids (into (filter misc/iid? references) (filter misc/iid? refd))
         rooturls (remove misc/iid? references)
         dispatches (map #(vector :dispatch [:load-opinion %]) iids)
         dispatches (if (empty? rooturls)
                      dispatches
                      (cons [:dispatch [:load-rooturls rooturls
                                        :no-text true
                                        :no-references true]]
                            dispatches))]
     {:db (-> db
              (assoc-in [:references key] references)
              (assoc-in [:refd key] refd))
      ;;FIXME: Might want a way to disable auto load
      ;;NOTE: Might switch to warpaks...
      :fx (into [] dispatches)})))

(rf/reg-event-fx
 ::received-opinion-tree
 (fn [{:keys [db]} [_ key result]]
   (let [result (cljs.reader/read-string result)
         focus (:focus-id db)
         db (assoc-in db [:opinion-tree-store key] result)
         loadables (if focus (misc/get-sub-tree db [nil focus]) result)]
     {:db db
      :fx [ [:dispatch [:load-opinions (flatten loadables)]]]})))

(rf/reg-event-fx
 ::request-grouped
 (fn [{:keys [db]} _]
   {:http-xhrio {:method :get
                 :uri (data-url (:ipns-host db) "/subjective/default/grouped.data")
                 :timeout 18000
                 :response-format (ajax/text-response-format)
                 :on-success [::received-grouped]
                 :on-failure [::failure "default" "grouped"]}}))

(defn proc-grouped [data]
  (let [{:keys [groups keywords] :as grouped} (cljs.reader/read-string data)
        keywords (apply hash-map keywords)
        groups (map
                (fn [group] (map (partial apply hash-map) group))
                groups)]
    (assoc grouped :keywords keywords :groups groups)))

(rf/reg-event-fx
 ::received-grouped
 (fn [{:keys [db]} [_ result]]
   (let [data (proc-grouped result)
         {:keys [:group-opinions :group-rooturls]} data]
     {:db (assoc db :grouped data)
      :fx [ [:dispatch [:load-rooturls group-rooturls :no-text true :no-references true]]
            [:dispatch [:load-opinions group-opinions]]]})))

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

(defn merge-warstats [& ws]
  (let [refd (apply clojure.set/union (keep :refd ws))
        w (apply merge ws)]
    (if (empty? refd)
      w
      (assoc w :refd refd))))

(rf/reg-event-db
 ::complete-debounce
 (fn [db _]
   (assoc
    db
    :warstats-store (misc/merge-map merge-warstats (:warstats-store db) (::warstats-tmp db))
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
