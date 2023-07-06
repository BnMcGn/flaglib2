(ns flaglib2.displayables
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.titlebar :as tb]
   [re-com-tailwind.core :as rc]))

(defn root-title [& {:keys [url title display-depth intro-text hide-warstats
                            warstats hide-reply hide-count reply-excerpt reply-offset
                            hide-external-link warflagger-link children]}]
  (let [warstats (or warstats @(rf/subscribe [:warstats-store url]))
        class (str (nth deco/display-depths display-depth)
                   " "
                   ;;FIXME: do we add <a> text decoration stuff here? See target-title in css
                   ((mood/flavor-from-own-warstats warstats) deco/flavor-background))]
    [rc/h-box
     :class class
     :align :center
     :children
     [intro-text
      [tb/headline :title title :rootid url :url url]
      (when (and url (not hide-external-link))
        [tb/display-external-link :url url])
      (when-not hide-warstats
        [tb/display-warstats :warstats warstats])
      children
      (when-not hide-reply
        [tb/reply-link :url url :excerpt reply-excerpt :offset reply-offset])
      (when-not hide-count
        [tb/reply-count :warstats warstats])]]))

(defn root-title-mobile [& {:keys [url title display-depth intro-text hide-warstats
                            warstats hide-reply hide-count reply-excerpt reply-offset
                            hide-external-link warflagger-link children]}]
  (let [warstats (or warstats @(rf/subscribe [:warstats-store url]))
        class (str (nth deco/display-depths display-depth)
                   " grid-cols-2 grid "
                   ;;FIXME: do we add <a> text decoration stuff here? See target-title in css
                   ((mood/flavor-from-own-warstats warstats) deco/flavor-background))]
    [:div
     {:class class}
     intro-text
     [tb/headline :title title :rootid url :url url :class "col-span-2"]
     (when (and url (not hide-external-link))
       [tb/display-external-link :url url])
     (when-not hide-warstats
       [tb/display-warstats :warstats warstats :class "justify-self-end self-center"])
     children
     (when-not hide-reply
       [tb/reply-link :url url :excerpt reply-excerpt :offset reply-offset])
     (when-not hide-count
       [tb/reply-count :warstats warstats :class "justify-self-start"])]))

(defn root-title-short [& {:as params}]
  (reduce into
   [root-title
    :show-count false
    :hide-warstats true
    :hide-reply true
    :hide-external-link true
    :intro-text ""]
   (for [[k v] params]
     [k v])))

(defn popup-side [])

(defn- find-parent-hilited [element]
  (when element
    (if (= "hilited" (. element -className))
      element
      (recur (. element -parentElement)))))

(defn is-selection-in-single-hilited-text? [selection]
  (let [parent1 (find-parent-hilited (. selection -parentElement))]
    (and (not (. selection -isCollapsed))
         parent1
         (= parent1 (find-parent-hilited (. selection -focusNode))))))

(defn hilited-segment [])
(defn plain-segment [])
(defn parent-segment [])

;;NOTE: remember "hilited" class
(defn hilited-text [])
(defn hilited-text-core [])


(defn sub-opinion-list [])
(defn opinion-info [])
(defn excerptless-opinions [])
(defn opinion-summary [])


(defn thread-excerpt-display
  [& {:keys [leading-context trailing-context excerpt excerpt-class]}]
  (if (or leading-context trailing-context)
    [:div
     {:class "thread-excerpt"}
     [:span (excerpts/rebreak leading-context)]
     [:span {:class excerpt-class} (excerpts/rebreak excerpt)]
     [:span (excerpts/rebreak trailing-context)]]
    [:div
     {:class "thread-excerpt thread-excerpt-unfound"}
     [:span {:class excerpt-class} (excerpts/rebreak excerpt)]]))

(defn thread-excerpt
  [& {:keys [opinion opinionid text]}]
  (let [opinion (or opinion
                    @(rf/subscribe [:opinion-store opinionid]))
        opid (or opinionid (:id opinion))
        {:keys [leading trailing excerpt]}
        (if (and opinion (:leading opinion))
          opinion
          (let [tpos (:text-position opinion)]
            (excerpts/excerpt-context text (nth tpos 0) (nth tpos 1))))]
    [thread-excerpt-display
     :leading-context leading :trailing-context trailing :excerpt excerpt
     :excerpt-class (mood/flavor+freshness @(rf/subscribe [:warstats-store nil]) opid)]))


(defn reference [])
(defn reference-default-display [])
(defn reference-excerpt-display [])
(defn question [])
(defn thread-opinion [])

(defn opinion-casual [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        text (if opinion
               (str "opinion by " (:authorname opinion))
               "unknown opinion")]
    [:span text]))
