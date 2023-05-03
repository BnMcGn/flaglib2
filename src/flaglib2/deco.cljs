(ns flaglib2.deco
  (:require
   [clojure.string :as str]))


;; formerly aside

(defn casual-note-heading [contents]
  [:h4
   {:class "m-0 bold italic font-[0.9rem] bg-gray-300 leading-4"}
   contents])


(def positive-magnitude '("#00ff0000" "#00ff0033" "#00ff0055" "#00ff0088" "#00ff00ff"))
(def negative-magnitude '("#ff000000" "#ff000033" "#ff000055" "#ff000088" "#ff0000ff"))

