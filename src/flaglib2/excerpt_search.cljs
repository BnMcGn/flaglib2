(ns flaglib2.excerpt-search
  (:require
   [re-com.core :as rc]
   [re-frame.core :as rf]
   [reagent.core :as reagent]
   
   [flaglib2.misc :as misc]
   [flaglib2.suggester :as suggester]
   [flaglib2.excerpts :as excerpts]))

;;FIXME: remove unused
;;(rf/reg-sub ::raw-excerpt-search :-> ::raw-excerpt-search)
(rf/reg-sub ::excerpt-start :-> ::excerpt-start)
;;(rf/reg-sub ::suggestions :-> ::suggestions)

(rf/reg-event-fx
 ::excerpt-start-selected
 (fn [{:keys [db]} [_ location start]]
   {:db (assoc db ::excerpt-start start)
    :fx [[:dispatch [:flaglib2.suggester/reset location]]]}))

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
      (assoc db ::raw-excerpt-search search ::excerpt-start xstart)
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

(defn render-suggestion [tdat item]
  (let [start @(rf/subscribe [::excerpt-start])]
    (if start
      (render-end-suggestion tdat start item)
      (render-start-suggestion tdat item))))

(rf/reg-event-fx
 ::set-excerpt-start-or-end
 [misc/call-something]
 (fn [{:keys [db]} [_ & {:keys [tdat item endpoint]}]]
   (if-let [start? (::excerpt-start db)]
     {:call-something [endpoint (excerpts/start-end->excerpt-offset tdat start? item)]}
     {:fx [[:dispatch [::excerpt-start-selected [::excerpt-suggester] item]]]})))

;;FIXME: need to handle existing excerpt/offset
(defn excerpt-search [& {:as init}]
  "on-change can be an event or a function"
  (let [model (reagent/atom nil)]
    (fn [& {:keys [text excerpt on-change width]}]
      (let [start @(rf/subscribe [::excerpt-start])
            tdat (excerpts/create-textdata text)
            location [::excerpt-suggester]]
        [rc/v-box
         :class "rc-typeahead"
         :width width
         :children
         [[rc/input-text
           :model model
           :on-change (fn [itm]
                        (reset! model itm)
                        (rf/dispatch [::do-search itm tdat]))
           :change-on-blur? false
           :attr {:on-key-down (partial suggester/suggester-keydown-handler! location)
                  :on-focus #()
                  ;;FIXME:
                  ;;:on-blur ???
                  }]
          (suggester/suggester
           :location location
           :on-select #(rf/dispatch [::set-excerpt-start-or-end
                                     :tdat tdat
                                     :item %1
                                     :endpoint on-change])
           :render-suggestion #(render-suggestion tdat %1))]]))))


