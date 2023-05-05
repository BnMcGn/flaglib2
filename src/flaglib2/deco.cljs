(ns flaglib2.deco
  (:require
   [clojure.string :as str]))


;; formerly aside

(defn casual-note-heading [contents]
  [:h4
   {:class "m-0 bold italic font-[0.9rem] bg-gray-300 leading-4"}
   contents])


(def positive-magnitude
  '("bg-[#00ff0000]" "bg-[#00ff0033]" "bg-[#00ff0055]" "bg-[#00ff0088]" "bg-[#00ff00ff]"))
(def negative-magnitude
  '("bg-[#ff000000]" "bg-[#ff000033]" "bg-[#ff000055]" "bg-[#ff000088]" "bg-[#ff0000ff]"))

(def display-depths ["ml-0"
                     "ml-[2em]"
                     "ml-[4em]"
                     "ml-[6em]"
                     "ml-[8em]"
                     "ml-[10em]"
                     "ml-[10.3em]"
                     "ml-[10.45em]"
                     "ml-[10.6em]"
                     "ml-[10.75em]"])


(def flavor-background {:contested "bg-[#ffc380]"
                        :positive "bg-[#80ff80]"
                        :negative "bg-[#ff8080]"
                        :neutral "bg-[#bce0e2]"})
