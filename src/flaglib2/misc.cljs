(ns flaglib2.misc
  (:require
   [cljs-time.format]
   [re-frame.registrar]
   [clojure.string :as str]))

(defn encode-uri-component2 [uri]
  (let [ichars ":\",()/\\%?="]
    (apply str
           (map (fn [itm]
                  (if (= -1 (.indexOf ichars itm))
                    itm
                    (str "%" (.toUpperCase (.toString (.charCodeAt itm 0) 16)))))
                (seq uri)))))

(def formatter (cljs-time.format/formatters :date-time-no-ms))
(defn parse-time [timestamp]
  (cljs-time.format/parse formatter timestamp))

(def whitespace-characters #{\space \newline \backspace \tab \formfeed \return})

(def url-pattern #"(?i)^(?:(?:https?|ftp)://)(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)(?:\.(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)*(?:\.(?:[a-z\u00a1-\uffff]{2,}))\.?)(?::\d{2,5})?(?:[/?#]\S*)?$")

(defn url? [item]
  (re-matches url-pattern item))

(defn url-domain [url]
  (let [domain (-> url
                   (str/split "//")
                   second
                   (str/split "/")
                   first)]
    (if (str/starts-with? domain "www.")
      (subs domain 4)
      domain)))

(defn first-index [itm coll & {:keys [test] :or {test =}}]
  (first (keep-indexed #(when (test %2 itm) %1) coll)))

(defn list-events []
  (let [store @re-frame.registrar/kind->id->handler]
    (keys (:event store))))

(defn list-subs []
  (let [store @re-frame.registrar/kind->id->handler]
    (keys (:sub store))))

(defn list-fx []
  (let [store @re-frame.registrar/kind->id->handler]
    (keys (:fx store))))

(defn say [itm]
  (do (println itm)
      itm))



(defn reformat-urls-lists [lists]
  (for [l lists
        [k urls] l
        url (or urls [])]
    {:url url :category k}))

(defn reformat-urls-lists-simple [lists]
  (for [l lists
        [k urls] (or l {})
        url (or urls [])]
    url))
