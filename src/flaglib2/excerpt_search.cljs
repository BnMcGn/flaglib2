(ns flaglib2.excerpt-search
  (:require
   [re-com-tailwind.core :as rc]
   [re-frame.alpha :as rf]
   [reagent.core :as reagent]

   [re-com-tailwind.functions :refer [tw-btn-primary]]

   [flaglib2.misc :as misc]
   [flaglib2.suggester :as suggester]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.displayables :as disps]
   [flaglib2.fabricate :as fabricate]
   [flaglib2.stepper :as step]))

(rf/reg-sub ::raw-excerpt-search :-> ::raw-excerpt-search)
(rf/reg-sub ::excerpt-start :-> ::excerpt-start)
(rf/reg-sub ::excerpt-end :-> ::excerpt-end)
(rf/reg-sub ::suggestions :-> (fn [db] (get-in db [::excerpt-suggester :suggestions])))
(rf/reg-sub ::debouncing :-> ::debouncing)
(rf/reg-sub ::tdat :-> ::tdat)

(rf/reg-event-fx
 ::excerpt-start-selected
 (fn [{:keys [db]} [_ location start]]
   {:db (assoc db ::excerpt-start start)
    :fx [[:dispatch [::suggester/reset location]]]}))

(rf/reg-event-db
 ::do-search
 (fn [db [_ search tdat]]
   (let [start (::excerpt-start db)
         [xstart suggests]
         (cond
           (and start (not (excerpts/excerpt-start-valid? tdat search start)))
           [nil []]
           (empty? search)
           [nil []]
           start
           (let [[starts ends] (excerpts/excerpt-possibilities tdat search start)]
             [(first starts) ends])
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
 :<- [::excerpt-end]
 :<- [::suggestions]
 :<- [::debouncing]
 (fn [[raw-search start end suggestions debouncing] _]
   (if (empty? raw-search)
     :empty
     (if start
       (cond
         (empty? suggestions)
         (if (= 0 (:remaining start))
           :complete
           (if end
             :complete
             (if (excerpts/search-tail-is-whitespace? raw-search start)
               :started
               :failed)))
         (= 1 (count suggestions))
         :complete
         :else
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
 (fn [{:keys [db]} [_ & {:keys [item endpoint location]}]]
   (if item
     (if-let [start? (::excerpt-start db)]
       {:db (assoc db ::excerpt-end item)
        :call-something [endpoint (excerpts/start-end->excerpt-offset (::tdat db) start? item)]
        :fx [[:dispatch [::suggester/reset location]]]}
       {:db (assoc db ::excerpt-end nil)
        :fx [[:dispatch [::excerpt-start-selected [::excerpt-suggester] item]]]})
     {:call-something [endpoint nil]})))

(rf/reg-event-db
 ::init-excerpt-offset
 (fn [db [_ tdat excerpt offset]]
   (let [newdb (assoc db ::tdat tdat)]
     (if (not-empty excerpt)
       (assoc newdb ::excerpt-start (excerpts/excerpt-offset->start tdat excerpt offset))
       (assoc newdb ::excerpt-start nil)))))


;;FIXME: Don't have a way to input offset in case of unmatched excerpt
(defn excerpt-search
  "on-change can be an event or a function"
  [& {:keys [excerpt offset text]}]
  (let [model (reagent/atom (when (not-empty excerpt) excerpt))
        tdat (excerpts/create-textdata text)]
    (rf/dispatch-sync [::init-excerpt-offset tdat excerpt offset])
    (fn [& {:keys [on-change width]}]
      (let [location [::excerpt-suggester]]
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
                                     :location location
                                     :endpoint on-change])
           :render-suggestion #(render-suggestion %1)]]]))))

;;FIXME: Next few items might do better in a higher level file. Refers to stepper
(rf/reg-event-fx
 ::accept-entry
 (fn [{:keys [db]} [_ status]]
   {:db (assoc db ::fabricate/excerpt
               (cond
                 (= status :failed)
                 [(::raw-excerpt-search db) nil]
                 (= status :empty)
                 [nil nil]
                 (::excerpt-start db)
                 (excerpts/start-end->excerpt-offset
                  (::tdat db) (::excerpt-start db) (::excerpt-end db))
                 :else
                 [nil nil]))
    :fx [ [:dispatch [::step/goto :opine]]]}))

(defn excerpt-search-buttons []
  (let [status @(rf/subscribe [::excerpt-search-status])]
    [step/button-box
     (step/button-spacer
      nil
      (case (or status :empty)
        :empty
        [[rc/button :label "Accept" :class (tw-btn-primary)
          :on-click #(rf/dispatch [::accept-entry :empty])]]
        :complete
        [[rc/button :label "Accept" :class (tw-btn-primary)
          :on-click #(rf/dispatch [::accept-entry :complete])]]
        (:started :unstarted :failed)
        [[rc/button
          :label "Accept as Entered"
          :class (tw-btn-primary)
          :on-click #(rf/dispatch [::accept-entry status])]]))]))

(rf/reg-sub
 ::active-excerpt
 :<- [::excerpt-start]
 :<- [::excerpt-end]
 :<- [::tdat]
 (fn [[start end tdat] _]
   (if start
     (excerpts/start-end->excerpt-offset tdat start end)
     [nil nil])))

;;FIXME: might not be accurate when bad excerpt comes from server parameters
(defn excerpt-search-context []
  (let [status @(rf/subscribe [::excerpt-search-status])
        tdat @(rf/subscribe [::tdat])
        [excerpt offset] (or @(rf/subscribe [::active-excerpt]) [nil nil])
        raw @(rf/subscribe [::raw-excerpt-search])]
    (if excerpt
      (case status
       :empty
       ""
       :failed
       [disps/thread-excerpt-display :chunks [["bg-orange-400" excerpt]] :status :not-found]
       (:started :unstarted :complete)
       (let [{:keys [leading trailing]} (excerpts/excerpt-context2 tdat excerpt offset)]
         [disps/thread-excerpt-display
          (excerpts/make-excerpt-chunks-from-opinion
           {:excerpt excerpt
            :leading-context leading
            :trailing-context trailing}
           :excerpt-class "bg-orange-400")]))
      (when-not (empty? raw)
        [disps/thread-excerpt-display :chunks [["" raw]] :status :not-found]))))
