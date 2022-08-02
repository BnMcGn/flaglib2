(ns flaglib2.excerpt-search
  (:require
   [re-com.core :as rc]
   [re-frame.core :as rf]
   [reagent.core :as reagent]
   
   [flaglib2.misc :as misc]
   [flaglib2.suggester :as suggester]
   [flaglib2.excerpts :as excerpts]))

(rf/reg-sub ::raw-excerpt-search :-> ::raw-excerpt-search)
(rf/reg-sub ::excerpt-start :-> ::excerpt-start)
(rf/reg-sub ::suggestions :-> (fn [db] (get-in db [::excerpt-suggester :suggestions])))
(rf/reg-sub ::debouncing :-> ::debouncing)

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

(rf/reg-event-fx
 ::debounced-do-search
 (fn [{:keys [db]} [_ search tdat]]
   (if (::debouncing db)
     {:db (assoc db ::current-search [search tdat])}
     {:db (assoc db ::current-search [search tdat] ::debouncing true)
      :fx [ [:dispatch-later {:ms 500 :dispatch [::complete-do-search]}]]})))

(rf/reg-event-fx
 ::complete-do-search
 (fn [{:keys [db]} _]
   (let [curr (::current-search db)]
     {:db (assoc db ::debouncing false)
      :fx [ [:dispatch (into [] (flatten [::do-search curr]))]]})))

(rf/reg-sub
 ::excerpt-search-status
 :<- [::raw-excerpt-search]
 :<- [::excerpt-start]
 :<- [::suggestions]
 :<- [::debouncing]
 (fn [[raw-search start suggestions debouncing] _]
   (if (empty raw-search)
     :empty
     (if start
       (cond
         (empty? suggestions)
         (if (= 0 (:remaining start))
           :complete
           :failed)
         (= 1 (count suggestions))
         :complete
         :else
         ;;FIXME: Excerpt-search module does not track selection of end, only calling the
         ;; externally supplied callback. So might be :complete instead of :started but
         ;; we can't tell from here. May need to rework if this turns out to be a problem.
         :started)
       (cond
         (empty? suggestions)
         (if debouncing
           :unstarted
           :failed)
         (= 1 (count suggestions))
         (if (= 0 (:remaining (nth suggestions 0)))
           :complete
           :unstarted)
         :else
         :unstarted)))))

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

(defn render-suggestion [item]
  (let [start @(rf/subscribe [::excerpt-start])
        tdat @(rf/subscribe [::tdat])]
    (if start
      (render-end-suggestion tdat start item)
      (render-start-suggestion tdat item))))

(rf/reg-event-fx
 ::set-excerpt-start-or-end
 [misc/call-something]
 (fn [{:keys [db]} [_ & {:keys [item endpoint]}]]
   (if-let [start? (::excerpt-start db)]
     {:db (assoc db ::excerpt-end item)
      :call-something [endpoint (excerpts/start-end->excerpt-offset (::tdat db) start? item)]}
     {:db (assoc db ::excerpt-end nil)
      :fx [[:dispatch [::excerpt-start-selected [::excerpt-suggester] item]]]})))

(rf/reg-event-db
 ::init-excerpt-offset
 (fn [db [_ tdat excerpt offset]]
   (let [newdb (assoc db ::tdat tdat)]
     (if (not-empty excerpt)
       (assoc newdb ::excerpt-start (excerpts/excerpt-offset->start tdat excerpt offset))
       newdb))))


;;FIXME: Don't have a way to input offset in case of unmatched excerpt
(defn excerpt-search [& {:as init :keys [excerpt offset text]}]
  "on-change can be an event or a function"
  (let [model (reagent/atom (when (not-empty excerpt) excerpt))
        tdat (excerpts/create-textdata text)]
    (rf/dispatch-sync [::init-excerpt-offset tdat excerpt offset])
    (fn [& {:keys [text on-change width]}]
      (let [start @(rf/subscribe [::excerpt-start])
            location [::excerpt-suggester]]
        [rc/v-box
         :class "rc-typeahead"
         :width width
         :children
         [[rc/input-text
           :model model
           :on-change (fn [itm]
                        (reset! model itm)
                        (rf/dispatch [::debounced-do-search itm tdat]))
           :change-on-blur? false
           :attr {:on-key-down (partial suggester/suggester-keydown-handler! location)
                  :on-focus #()
                  ;;FIXME:
                  ;;:on-blur ???
                  }]
          [suggester/suggester
           :location location
           :on-select #(rf/dispatch [::set-excerpt-start-or-end
                                     :item %1
                                     :endpoint on-change])
           :render-suggestion #(render-suggestion %1)]]]))))

;;FIXME: Next few items might do better in a higher level file. Refers to stepper
(rf/reg-event-fx
 ::accept-entry
 (fn [{:keys [db]} _]
   {:fx [ [:dispatch [:flaglib2.stepper/set-summary :excerpt]]]}))

(defn excerpt-search-buttons []
  (let [status @(rf/subscribe [::excerpt-search-status])]
    (case status
      :empty
      [[rc/button :label "Accept" :disabled? true]]
      :complete
      [[rc/button :label "Accept" :on-click #(rf/dispatch [::accept-entry])]]
      (:started :unstarted :failed)
      [[rc/button :label "Accept as Entered" :on-click #(rf/dispatch [::accept-entry])]])))

(rf/reg-sub
 ::active-excerpt
 :<- [::excerpt-start]
 :<- [::excerpt-end]
 :<- [::tdat]
 (fn [[start end tdat] _]
   (if start
     (excerpts/start-end->excerpt-offset tdat start end)
     [nil nil])))

(defn excerpt-search-context []
  (let [status @(rf/subscribe [::excerpt-search-status])
        text @(rf/subscribe [:flaglib2.fabricate/active-text])
        [excerpt offset] @(rf/subscribe [::active-excerpt])]
    ))
