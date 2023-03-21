(ns flaglib2.deco
  (:require
   [clojure.string :as str]))


;; formerly aside

(defn casual-note-heading [contents]
  [:h4
   {:class "m-0 bold italic font-[0.9rem] bg-gray-300 leading-4"}
   contents])
