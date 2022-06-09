(ns flaglib2.excerpt-search
  (:require
   [re-com.core :as rc]
   [re-frame.core :as rf]
   
   [flaglib2.misc :as misc]
   [flaglib2.suggester :as suggester]
   [flaglib2.excerpts :as excerpts]))

(rf/reg-sub ::raw-excerpt-search :-> ::raw-excerpt-search)
(rf/reg-sub ::excerpt-start :-> ::excerpt-start)
(rf/reg-sub ::suggestions :-> ::suggestions)

(rf/reg-event-db
 ::do-search
 (fn [db [_ search tdat]]
   (let [start (::excerpt-start db)
         [xstart suggests]
         (cond
           (and start (not (excerpts/excerpt-start-valid? tdat search start)))
           [nil []]
           (zero? (count search))
           [nil []]
           start
           [start (get (excerpts/excerpt-possibilities tdat search start) 1)]
           :else
           (let [[starts ends] (excerpts/excerpt-possibilities tdat search)]
             (if (= 1 (count starts))
               [(nth starts 0) ends]
               [nil starts])))]
     (assoc-in
      (assoc db ::raw-excerpt-search search ::excerpt-start start)
      [::excerpt-suggester :suggestions] suggests))))

(defn render-start-suggestion [tdat start]
  (let [text (excerpts/clean-string-for-excerpt
              (subs (:text tdat) (:start-index start) (inc (:end-index start))))
        context (excerpts/clean-string-for-excerpt
                 (subs (:text tdat) (inc (:end-index start))
                       (min (+ 10 (:end-index start)) (dec (:text-length tdat)))))]
    [:span (str (:start-index start) ": ") [:strong text] context]))

(defn render-end-suggestion [tdat start end]
  (let [text (excerpts/clean-string-for-excerpt
              (subs (:text tdat) (:start-index start) (inc (:end-index end))))]
    [:span (str (count text) " chars:" text)]))

;;FIXME: need to handle existing excerpt/offset
(defn excerpt-search [& {:as init}]
  (let [model (atom nil)]
    (fn [& {:keys [text excerpt on-change width]}]
         (let [start @(rf/subscribe [::excerpt-start])
               suggestions @(rf/subscribe [::suggestions])
               tdat (excerpts/create-textdata text)]
           [rc/v-box
            :class "rc-typeahead"
            :width width
            :children
            [[rc/input-text
              ;;FIXME: that's broken:
              :model @model
              :on-change (fn [itm]
                           (rf/dispatch [::do-search itm tdat])
                           (reset! model itm))
              :change-on-blur? false
              :attr {:on-key-down (partial suggester/suggester-keydown-handler! [::excerpt-suggester])
                     :on-focus #()
                     ;;FIXME:
                     ;;:on-blur ???
                     }]
             [suggester/suggester
              :location [::excerpt-suggester]
              :suggestions suggestions
              :on-select #(when start (on-change (excerpts/start-end->excerpt-offset tdat start %1)))
              :render-suggestion #(if start
                                    (render-end-suggestion tdat start %1)
                                    (render-start-suggestion tdat %1))]]]))))




