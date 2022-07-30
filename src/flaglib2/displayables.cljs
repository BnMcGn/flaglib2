(ns flaglib2.displayables
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.excerpts :as excerpts]
   [re-com.core :as rc]))



(defn thread-excerpt-display
  [& {:keys [leading-context trailing-context excerpt excerpt-class]}]
  (if (or leading-context trailing-context)
    [:div
     :class "thread-excerpt"
     [:span (excerpts/rebreak leading-context)]
     [:span :class excerpt-class (excerpts/rebreak excerpt)]
     [:span (excerpts/rebreak trailing-context)]]
    [:div
     :class "thread-excerpt thread-excerpt-unfound"
     [:span :class excerpt-class (excerpts/rebreak excerpt)]]))

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
