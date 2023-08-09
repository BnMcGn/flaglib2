(ns flaglib2.deco
  (:require
   [clojure.string :as str]))


;; formerly aside

(defn casual-note-heading [contents]
  [:h4
   {:class "m-0 bold italic font-[0.9rem] bg-gray-300 leading-4"}
   contents])

(defn casual-heading [contents]
  [:h3
   {:class "m-0 bold italic bg-gray-300 leading-4"}
   contents])

(def pos-mag-0 "bg-[#00ff0000]")
(def pos-mag-1 "bg-[#00ff0033]")
(def pos-mag-2 "bg-[#00ff0055]")
(def pos-mag-3 "bg-[#00ff0088]")
(def pos-mag-4 "bg-[#00ff00ff]")

(def neg-mag-0 "bg-[#ff000000]")
(def neg-mag-1 "bg-[#ff000033]")
(def neg-mag-2 "bg-[#ff000055]")
(def neg-mag-3 "bg-[#ff000088]")
(def neg-mag-4 "bg-[#ff0000ff]")

;; Weird hack to get around tailwindcss failure
(def positive-magnitude [pos-mag-0 pos-mag-1 pos-mag-2 pos-mag-3 pos-mag-4])
(def negative-magnitude [neg-mag-0 neg-mag-1 neg-mag-2 neg-mag-3 neg-mag-4])

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

(def patch "border-dashed border border-slate-800")

(def button-disabled "text-gray-500")
