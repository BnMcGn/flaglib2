(ns flaglib2.titlebar
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
;   [clojure.string :as string]
;   [clojure.walk :as walk]
   [flaglib2.misc :as misc]
   [flaglib2.flags :as flags]
   [re-com.core :as rc]))




(defn flag-icon [type]
  (let [flag (get type flags/flags)]
    (str "/static/img/small/wf_flag-" (subs (:color flag) 1) ".svg")))
