(ns flaglib2.target-summary
  (:require
   [re-frame.alpha :as rf]
   [reagent.core :as r]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.flags :as flags]
   [flaglib2.titlebar :as tb]
   [flaglib2.displayables :as disp]
   [flaglib2.mood :as mood]))

(defn translate [x y]
  (str "translate(" x "," y ")"))

(def bar-width 20)
(def bar-height-max 150)
(def m [0 0 0 0])
(def centerline (+ bar-height-max 30))

(defn positive-bar [& {:keys [style label transform height title]}]
  (let [top (- centerline height)]
    [:g
     {:transform transform
      :width bar-width}
     [:rect
      {:height height
       :width bar-width
       :x 0
       :y top
       :style style}
      [:title title]]
     [:text
      {:x (* 0.5 bar-width)
       :y (- top 10)
       :text-anchor "middle"}
      label]]))

(defn negative-bar [& {:keys [style label transform height title]}]
  [:g
   {:transform transform}
   [:rect
    {:height height
     :width bar-width
     :x 0
     :y 0
     :style style}
    [:title title]]
   [:text
    {:x (* 0.5 bar-width)
     :y (+ height 23)
     :text-anchor "middle"}
    label]])

(defn summary-scores-chart [targetid]
  (let [w (- 110 (m 1) (m 3))
        h (- (* 2 centerline) (m 0) (m 2))
        warstats @(rf/subscribe [:warstats-store targetid])
        colsize 20
        col2 70
        x-right (+ (int (:x-right warstats)) (int (:x-right-refs warstats)))
        x-wrong (+ (int (:x-wrong warstats)) (int (:x-wrong-refs warstats)))
        x-up (+ (int (:x-up warstats)) (int (:x-up-refs warstats)))
        x-down (+ (int (:x-down warstats)) (int (:x-down-refs warstats)))
        highest (max x-right x-wrong x-up x-down)
        height-range-adjust
        (fn [val]
          (max 1 (misc/as-in-range
                  0 bar-height-max
                  (misc/relative-to-range 0 highest val))))]
    [:div {:class "float-left"}
     [:h3 "Score"]
     (when-not (empty? warstats)
       [:div
        {:class "border-[3px] border-black"}
        [:svg {:width (+ w (m 1) (m 3))
               :height (+ h (m 0) (m 2))
               :transform (translate (m 3) (m 0))}
         [positive-bar
          :style {:fill "#80ff80" :stroke "black" :strokeWidth "1"}
          :label x-up
          :title "Up"
          :height (height-range-adjust x-up)
          :transform (translate colsize 0)]
         [positive-bar
          :style {:fill "#80ff80" :stroke "black" :strokeWidth "1"}
          :label x-right
          :title "Right"
          :height (height-range-adjust x-right)
          :transform (translate col2 0)]
         [negative-bar
          :style {:fill "#ff8080" :stroke "black" :strokeWidth "1"}
          :label x-down
          :title "Down"
          :height (height-range-adjust x-down)
          :transform (translate colsize centerline)]
         [negative-bar
          :style {:fill "#ff8080" :stroke "black" :strokeWidth "1"}
          :label x-wrong
          :title "Wrong"
          :height (height-range-adjust x-wrong)
          :transform (translate col2 centerline)]]])]))

(defn display-summary-word [targetid & {:keys [class]}]
  (let [db @(rf/subscribe [:core-db])
        warstats @(rf/subscribe [:warstats-store targetid])
        word (mood/in-a-word warstats :db db :key targetid)]
    [:span
     {:class class}
     ({:positive "Positive"
       :significant "Significant"
       :negative "Negative"
       :restricted "Restricted"
       :sidelined "Sidelined"
       :contested "Contested"
       :unsupported "Unsupported"
       :awkward "Awkward"
       :ignored "Ignored"}
      word)]))

(defn display-other-flags [targetid & {:keys [hide-inactive]}]
  (let [warstats @(rf/subscribe [:warstats-store targetid])]
    [:div [:h3 "Flags"]
     (when warstats
       (into [:div
             {:class "grid gap-1 grid-cols-2 sm:grid-cols-1 text-xs sm:text-sm"}]
            (for [flag flags/other-flags
                  :let [flinfo (flags/flags flag)]
                  :when (not (and hide-inactive (not (warstats flag))))]
              [:div
               {:class "border-[2px] rounded sm:w-44 w-36 flex justify-center"
                :style (cond
                         (not (warstats flag)) {:background-color "lightgrey"
                                                :color "#333"
                                                :border-color "grey"}
                         (< 5 (warstats flag)) {:background-color (str (flinfo :color) "99")
                                                :border-color (flinfo :color)}
                         :else {:background-color "white"
                                :border-color (str (flinfo :color) "bb")})}
               (:label flinfo)])))]))

(defn warn-off-toggle [key]
  (let [vis @(rf/subscribe [:visibility key])
        iid (misc/iid? key)
        warnoff (:warn-off vis)
        excerpts (:warn-off-excerpt-only vis)
        override (:warn-off-override vis)
        typedesc (if iid "opinion" "article")
        segdesc (if excerpts "has restricted portions" "is restricted")
        model (r/atom override)]
    (when (or warnoff override)
      [:div
       {:class "border-[3px] border-black justify-center content-center justify-items-center"
        :style {:display "grid"}}
       [:p (str "This " typedesc " " segdesc ".")]
       [:p "Restore visibility:"]
       [rc/checkbox
        :model model
        :on-change (fn [new-value]
                     (if new-value
                       (rf/dispatch [:set-warn-off-override key])
                       (rf/dispatch [:remove-warn-off-override key])))]])))

(defn reply-count-long [target]
  (let [warstats @(rf/subscribe [:warstats-store target])
        opinions @(rf/subscribe [:opinion-store])
        children @(rf/subscribe [:immediate-children target])
        immediate (:replies-immediate warstats)
        total (:replies-total warstats)
        excerpts (count
                  (for [iid children
                        :let [opin (get opinions iid)]
                        :when (:excerpt opin)]
                    opin))]
    [:div [:h3 "Replies"]
     [:div {:class "grid w-44" :style {:grid-template-columns "90% 10%"}}
      [:span "Direct responses:"] [:span immediate]
      [:span "In conversation:"] [:span total]
      [:span "As excerpts:"] [:span excerpts]]]))

(defn score-contribution-section [label items wstore]
  (let [db @(rf/subscribe [:core-db])
        small @(rf/subscribe [:window-small?])
        fields (if small
                 [:effect :opinion-icon :author-long]
                 [:effect :opinion-icon :flag-name :date-stamp :author-long :warstats])
        items (sort-by #(get-in wstore [% :effect] 0) items)]
    (into [:div [:h4 label]]
          (for [i items
                :let [tbstuff (tb/opinion-tb-stuff i db)]]
            (into [:div {:class (:bg-color tbstuff)}]
                  (tb/assemble-bar-parts tbstuff fields))))))

(defn score-contributions [targetid]
  (let [warstats @(rf/subscribe [:warstats-store targetid])
        wstore @(rf/subscribe [:warstats-store])
        ostore @(rf/subscribe [:opinion-store])
        children @(rf/subscribe [:immediate-children targetid])
        other-flags (into #{} flags/other-flags)
        children (for [c children
                       :let [opinion (get ostore c)
                             wstat (get wstore c)]
                       :when (and (other-flags (:flag opinion))
                                  (> 0 (get wstat :effect 0)))]
                   c)
        {:keys [x-up-source x-down-source x-right-source x-wrong-source]} warstats]

    (when-not (every?
               zero?
               (map
                count
                [x-up-source x-down-source x-right-source x-wrong-source children]))
      [:div
       [:h3 "Score Contributions"]
       (when-not (empty? x-up-source)
         [score-contribution-section "Up" x-up-source])
       (when-not (empty? x-down-source)
         [score-contribution-section "Down" x-down-source])
       (when-not (empty? x-right-source)
         [score-contribution-section "Right" x-right-source])
       (when-not (empty? x-wrong-source)
         [score-contribution-section "Wrong" x-wrong-source])
       (when-not (empty? children)
         [score-contribution-section "Flags" children])])))

(defn references-summary [targetid]
  (let [references @(rf/subscribe [:reference-opinions targetid])
        opstore @(rf/subscribe [:opinion-store])]
    (when-not (empty? references)
      [:div
       [:h3 "References Made"]
       (into [:div {:class "child:p-1"}]
             (for [r references
                   :let [opinion (opstore r)]]
               [disp/tree-address-container
                {}
                :tree-address (:tree-address opinion)
                :body
                [disp/reference opinion :minify true]]))])))

(defn refd-summary [targetid]
  (let [refd @(rf/subscribe [:refd targetid])
        opstore @(rf/subscribe [:opinion-store])]
    (when-not (empty? refd)
      [:div
       [:h3 "Incoming References"]
       (into [:div {:class "child:p-1"}]
             (for [r refd]
               [disp/display-refd-root-pov r]))])))

(defn summary-score-val [db opid]
  (let [effect (get-in db [:warstats-store opid :effect])]
    (and effect (< 2 effect) effect)))

(defn summary-controversy-val [db opid]
  (let [controversy (get-in db [:warstats-store opid :controversy])]
    (and controversy (< 2 controversy) controversy)))

(defn organize-opinion-tree [optree score-func]
  (let [res (for [opid (flatten optree)
                  :let [score (score-func opid)]
                  :when score]
              [opid score])]
    (map first (sort-by second res))))

(defn high-scores [targetid]
  (let [db @(rf/subscribe [:core-db])
        optree (get-in db [:sub-tree targetid])
        opids (organize-opinion-tree optree (partial summary-score-val db))]
    (when-not (empty? opids)
      [:div
       [:h3 "High Scoring Replies"]
       (into [:div {:class "child:p-1"}]
             (for [o opids
                   :let [opinion (get-in db [:opinion-store o])]]
               [disp/tree-address-container
                {}
                :tree-address (:tree-address opinion)
                :body
                [disp/opinion-summary o :hide-tree-address true :hide-icon true :hide-reply true]]))])))

(defn controversial [targetid]
  (let [db @(rf/subscribe [:core-db])
        optree (get-in db [:sub-tree targetid])
        opids (organize-opinion-tree optree (partial summary-controversy-val db))]
    (when-not (empty? opids)
      [:div
       [:h3 "Controversial Replies"]
       (into [:div {:class "child:p-1"}]
             (for [o opids
                   :let [opinion (get-in db [:opinion-store o])]]
               [disp/tree-address-container
                {}
                :tree-address (:tree-address opinion)
                :body
                [disp/opinion-summary o :hide-tree-address true :hide-icon true :hide-reply true]]))])))

(defn and-answers [qid]
  (let [opstore @(rf/subscribe [:opinion-store])
        replies @(rf/subscribe [:immediate-children qid])
        answers (keep #(misc/answer? (opstore %1)) replies)]
    (for [o answers
          :let [opinion (get opstore o)]]
      [disp/tree-address-container
       {}
       :tree-address (:tree-address opinion)
       :body
       [disp/opinion-summary o :hide-tree-address true :hide-icon true :hide-reply true]])))

(defn questions-and-answers [targetid]
  (let [warstats-store @(rf/subscribe [:warstats-store])
        optree @(rf/subscribe [:sub-tree targetid])
        questions (filter #(:question (warstats-store %1)) (flatten optree))]
    (when-not (empty? questions)
      [:div
       [:h3 "Questions and Answers"]
       (into [:div {:class "child:p-1"}]
             (for [q questions
                   r (cons [disp/question q :minify true] (and-answers q))]
               r))])))

(defn target-stats [& {:keys [rooturl]}]
  [:div
   [disp/root-title :url rooturl :intro-text "Article: " :display-depth 0]
   [:div {:class "flex flex-col gap-4 mb-8"}
     [:div
      {:class "flex flex-col sm:flex-row gap-4"}
      [summary-scores-chart rooturl]
      [display-other-flags rooturl]
      [warn-off-toggle rooturl]]
    [score-contributions rooturl]
    [references-summary rooturl]
    [refd-summary rooturl]
    [questions-and-answers rooturl]
    [high-scores rooturl]
    [controversial rooturl]]])

(defn opinion-stats [& {:keys [iid]}]
  [:div
   [disp/opinion-summary iid :hide-tree-address true]
   [:div {:class "flex flex-col gap-4 mb-8"}
    [:div
     {:class "flex flex-col sm:flex-row gap-4"}
     [summary-scores-chart iid]
     [display-other-flags iid]
     [warn-off-toggle iid]]
    [score-contributions iid]
    [references-summary iid]
    [refd-summary iid]
    [questions-and-answers iid]
    [high-scores iid]
    [controversial iid]]])
