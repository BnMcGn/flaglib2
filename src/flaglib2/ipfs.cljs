(ns flaglib2.ipfs
  (:require
   [goog.dom :as gdom]
   [re-frame.core :as rf]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [clojure.string :as string]
   [clojure.edn :as edn]
   [flaglib2.misc :as misc]))


(defn opinion-data-url [iid itype]
  (str "/ipns/" IPNSHOST "/opinions/" iid "/" itype ".data"))

(defn rooturl-data-url [rooturl rtype]
  (str "/ipns/" IPNSHOST "/rooturls/"
       (.replaceAll (misc/encode-uri-component2 rooturl) "%" "*")
       "/" rtype ".data"))

(reg-event-fx
 ::request-rooturl-item
 (fn [{:keys [db]} [rooturl resource-type]]
   ;;FIXME: could add indicator to db that request is pending...
   {:http-xhrio {:method :get
                 :uri (rooturl-data-url rooturl resource-type)
                 :timeout 6000
                 :response-format (ajax/text-response-format)
                 :on-success
                 [(keyword 'flaglib2.ipfs (string/join ["received-rooturl-" resource-type]))
                  rooturl]
                 ;; :on-failure []
                 }}))

(reg-event-fx
 ::received-rooturl-warstats
 (fn [{:keys [db]} [_ rooturl]]
   {:db
    (assoc db
           ::warstats-tmp (assoc (::warstats-tmp db) rooturl ))
    :fx [ [:dispatch [::start-debounce]] ]}))

(reg-event-fx
 ::start-debounce
 (fn [{:keys [db]} _]
   (if (::debouncing db)
     {}
     {:db (assoc db ::debouncing true)
      :fx [ [:dispatch-later {:ms 500 :dispatch [::complete-debounce]}] ]})))

(reg-event-db
 ::complete-debounce
 (fn [db _]
   {:warstats-store (merge (:warstats-store db) (::warstats-tmp db))
    ::warstats-tmp {}
    :text-store (merge (:text-store db) (::text-tmp db))
    ::text-tmp {}
    :title-store (merge (:title-store db) (::title-tmp db))
    ::title-tmp {}
    :opinion-store (merge (:opinion-store db) (::opinion-tmp db))
    ::opinion-tmp {}
    ::debouncing nil}))

(reg-event-db
 ::received-rooturl-warstats
 (fn [db [_ rooturl]]
   {::warstats-tmp
    (assoc (::warstats-tmp db) )}))

(reg-event-fx
 :load-rooturl
 (fn [_ [_ rooturl & {:keys [no-text]}]]
   {:fx [ [::request-rooturl-item rooturl "warstats"]]}))


