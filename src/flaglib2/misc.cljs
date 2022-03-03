(ns flaglib2.misc
  (:require
   [cljs-time.format]))

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

(def url-pattern #"(?i)^(?:(?:https?|ftp)://)(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)(?:\.(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)*(?:\.(?:[a-z\u00a1-\uffff]{2,}))\.?)(?::\d{2,5})?(?:[/?#]\S*)?$")

(defn url? (item)
  (re-matches url-pattern item))
