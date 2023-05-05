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

;;FIXME: are url/rootid redundant? has been tested with opinionid? Might be an early misnomer...
;; root-title?
(defn target-title [& {:keys [url rootid opinionid title display-depth intro-text hide-warstats
                              warstats hide-reply hide-count reply-excerpt reply-offset
                              hide-external-link warflagger-link children]
                       :or {:intro-text "Target Page: "}}]
  (let [id (or rootid opinionid)
        warstats (or warstats @(rf/subscribe [:warstats-store id]))
        opinion (when opinionid @(rf/subscribe [:opinion-store opinionid]))
        class [(nth deco/display-depths (or display-depth
                                            (when opinion (count (:tree-address opinion)))))
               ;;FIXME: do we add <a> text decoration stuff here? See target-title in css
               ((mood/flavor-from-own-warstats) deco/flavor-background)]
        [:div
         {:class class}
         intro-text
         [tb/headline :title title :url warflagger-link]
         (when (and rootid (not hide-external-link))
           [tb/display-external-link :url url])
         (when-not hide-warstats
           [tb/display-warstats :warstats warstats])
         children
         (when-not hide-reply
           [tb/reply-link :url url :excerpt reply-excerpt :offset reply-offset])
         (when-not hide-count
           [tb/reply-count :warstats warstats])]]))

(defn target-title-short [])
(defn popup-side [])

(defn hilited-segment [])
(defn plain-segment [])
(defn parent-segment [])
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
