(ns flaglib2.titlebar
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
;   [clojure.walk :as walk]
   [goog.string :as string]

   [re-com-tailwind.functions :refer [tw-btn-default tw-btn]]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs :as ipfs]
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

(defn flag-name [opinion]
  (let [flag (get flags/flags (:flag opinion))]
    [:span (str (:category flag) " " (:label flag))]))

(defn flag-icon [type]
  (let [flag (get flags/flags type)
        ;;FIXME: unknown flag should have unique icon
        color (if flag (subs (:color flag) 1) "fff")]
    (str "/static/img/small/wf_flag-" color ".svg")))

;; Might not need, dependent on need for tooltip
;;(defn opinion-icon-core [])

(defn opinion-icon [opid & {:keys [style]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])]
    [:a
     {:href (misc/make-opinion-url opinion)
      :style (or style {})}
     [:img {:src (flag-icon (:flag opinion))}]]))

(defn display-tree-address [tree-address & {:keys [style class]}]
  [rc/h-box
   :style style
   :class class
   :children
   (rest
    (reduce into []
            (for [id tree-address]
              [" > " [opinion-icon id]])))])

;;FIXME: might want magnitude to adjust proportionately to other axes
(defn display-warstats [& {:keys [warstats class]}]
  [rc/h-box
   :class (str "mr-3 " class)
   :children
   (into []
         (map
          (fn [axis]
            (let [stat (get warstats axis)
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

(defn date-stamp [opinion]
  (let [[quantity unit] (misc/ago (:created opinion))]
    [:span (str quantity " " unit " ago")]))

(defn author-long [opinion]
  (let [auth (or (:authorname opinion) (:author opinion))]
    [:a {:style {:color "black"}
         :href (misc/make-author-url auth)} auth]))

(defn reply-link [& {:keys [url excerpt offset]}]
  [:form
   {:class "inline-block relative text-sm mr-3"
    ;;FIXME: Why doesn't form vertically center like other elements?
    :style {:top "0.5em"}
    :action "/opinion/" :method "GET"}
   [:input {:type "hidden" :name "target" :value (or url "")}]
   (when-not (empty? excerpt)
     [:input {:type "hidden" :name "excerpt" :value (js/encodeURIComponent excerpt)}])
   (when offset
     [:input {:type "hidden" :name "offset" :value (or offset "")}])
   (if (and excerpt (not (empty? excerpt)))
     [:input
      {:type "submit"
       :title (str "Reply to the excerpt: \"" excerpt "\"")
       :value "Reply to Excerpt"
       :class (tw-btn (tw-btn-default))}]
     [:input {:type "submit" :value "Reply" :class (tw-btn (tw-btn-default))}])])

(defn reply-count [& {:keys [warstats class]}]
  (let [immediate (:replies-immediate warstats)
        total (:replies-total warstats)]
    [:span {:class ["text-base mr-3" class]
            :title (str immediate " direct responses, " total " in conversation")}
     (when (and immediate total) (str " (" immediate "/" total ")"))]))

(defn display-external-link [& {:keys [url black]}]
  [:a {:href url :class "mr-3"}
   [:img {:src (if black "/static/img/white-external-link.svg" "/static/img/black-external-link.svg")
          :style {:min-width "22px"}
          :alt "Original article" :title "Original article"}]])

(defn headline [& {:keys [title url domain rootid opinionid class]}]
  (when (and rootid opinionid)
    (throw (js/Error. "Can only use one of rootid or opinionid")))
  (let [id (or rootid opinionid)
        tinfo (when id @(rf/subscribe [:title-store id]))
        [titl available? patch?] (cond
                                   title [title true false]
                                   (misc/has-title? tinfo)
                                   (if (misc/alternate-title? tinfo)
                                     [(:title tinfo) true true]
                                     [(:title tinfo) true false])
                                   :else [(cond rootid rootid
                                                opinionid ""
                                                :else "")
                                          false false])
        domain (when domain (str "(" domain ")"))
        class ["mx-3"
               class
               (if available? "text-lg" "italic font-thin truncate")
               (when (and (not domain) patch?) deco/patch)]
        core (if domain
               (if patch?
                 [[:span {:class deco/patch} titl] (str " " domain)]
                 [(str titl " " domain)])
               [titl])
        url (cond
              (string? url) url
              (true? url) (cond rootid (misc/make-target-url rootid)
                                opinionid (misc/make-opinion-url {:iid opinionid})
                                :else false)
              :else false)]
    (if url
      [:span {:class class} (into [:a {:href url}] core)]
      (into [:span {:class class}] core))))

;;FIXME: read from text-store?
(defn comment-summary [& {:keys [comment opinion truncate]}]
  (let [comment (or comment (:comment opinion) "")]
    [:span {:class (if truncate "truncate" "")} comment]))



