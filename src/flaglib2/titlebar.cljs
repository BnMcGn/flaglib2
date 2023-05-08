(ns flaglib2.titlebar
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
;   [clojure.string :as string]
;   [clojure.walk :as walk]
   [flaglib2.misc :as misc]
   [flaglib2.flags :as flags]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]

   [re-com-tailwind.core :as rc]))

(def indicator-names
  {:x-up "thumbs_up_sign"
   :x-down "thumbs_down_sign"
   :x-right "check_mark"
   :x-wrong "ballot_x"})

(def warstat-text
  {:x-up "Has approval"
   :x-down "Has disapproval"
   :x-right "Has supporting evidence"
   :x-wrong "Has contradicting evidence"})

(defn flag-name [])
(defn opinion-icon-core [])
(defn opinion-icon [])
(defn display-tree-address [])

;;FIXME: might want magnitude to adjust proportionately to other axes
(defn display-warstats [& {:keys [warstats]}]
  [rc/h-box :children
   (into []
         (map
          (fn [axis]
            (let [stat (get axis warstats)
                  mag (if (integer? stat) (mood/magnitude stat) 0)
                  opacity (when (or (not stat) (zero? stat)) " opacity-25")
                  mags (if (#{:x-up :x-right} axis)
                         deco/positive-magnitude
                         deco/negative-magnitude)]
              [:span
               {:class (str (nth mags mag) opacity)}
               [:img {:src (str "/static/img/" (get indicator-names axis) ".svg")
                      :style {:width "12px" :height "12px"}
                      :title (get warstat-text axis)}]]))
          '(:x-up :x-down :x-right :x-wrong)))])

(defn display-date-nicely [])
(defn date-stamp [])
(defn author-long [])

(defn reply-link [& {:keys [url excerpt offset]}]
  [:form
   :class "inline-block relative text-sm"
   :action "/opinion/" :method "GET"
   [:input {:type "hidden" :name "target" :value url}]
   (when excerpt
     [:input {:type "hidden" :name "excerpt" :value (js/encodeURIComponent excerpt)}])
   (when offset
     [:input {:type "hidden" :name "offset" :value offset}])
   (if excerpt
     [:input
      :type "submit"
      :title (str "Reply to the excerpt: \"" excerpt "\"")
      :value "Reply to Excerpt"]
     [:input :type "submit" :value "Reply"])])

(defn reply-count [& {:keys [warstats]}]
  (let [immediate (:replies-immediate warstats)
        total (:replies-total warstats)]
    [:span {:class "text-base"
            :title (str immediate " direct responses, " total " in conversation")}
     (str " (" immediate "/" total ")")]))

(defn display-external-link [& {:keys [url]}]
  [:a :href url
   [:img {:src "/static/img/white-external-link.svg"
          :alt "Original article" :title "Original article"}]])

(defn headline [& {:keys [title external-link domain rootid opinionid]}]
  (when (and rootid opinionid)
    (throw (js/Error. "Can only use one of rootid or opinionid")))
  (let [id (or rootid opinionid)
        tinfo (when id @(rf/subscribe [:title-store id]))
        title (or title (:title tinfo) " ")]
    [:span {:class "mx-4"} title]))

(defn comment-summary [])

(defn flag-icon [type]
  (let [flag (get flags/flags type)]
    (str "/static/img/small/wf_flag-" (subs (:color flag) 1) ".svg")))


