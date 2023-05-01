(ns flaglib2.titlebar
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
;   [clojure.string :as string]
;   [clojure.walk :as walk]
   [flaglib2.misc :as misc]
   [flaglib2.flags :as flags]
   [re-com-tailwind.core :as rc]))


(defn flag-name [])
(defn opinion-icon-core [])
(defn opinion-icon [])
(defn display-tree-address [])
(defn display-warstats [])
(defn display-date-nicely [])
(defn date-stamp [])
(defn author-long [])
(defn reply-link [])
(defn reply-count [])
(defn headline-core [])
(defn headline [])
(defn comment-summary [])

(defn flag-icon [type]
  (let [flag (get flags/flags type)]
    (str "/static/img/small/wf_flag-" (subs (:color flag) 1) ".svg")))


