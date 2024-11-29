(ns flaglib2.target-summary
  (:require
   [re-frame.core :as rf]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.flags :as flags]
   [flaglib2.displayables :as disp]))

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
        highest (max (:x-right warstats) (:x-wrong warstats)
                     (:x-up warstats) (:x-down warstats))
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
          :label (:x-up warstats)
          :title "Up"
          :height (height-range-adjust (:x-up warstats))
          :transform (translate colsize 0)]
         [positive-bar
          :style {:fill "#80ff80" :stroke "black" :strokeWidth "1"}
          :label (:x-right warstats)
          :title "Right"
          :height (height-range-adjust (:x-right warstats))
          :transform (translate col2 0)]
         [negative-bar
          :style {:fill "#ff8080" :stroke "black" :strokeWidth "1"}
          :label (:x-down warstats)
          :title "Down"
          :height (height-range-adjust (:x-down warstats))
          :transform (translate colsize centerline)]
         [negative-bar
          :style {:fill "#ff8080" :stroke "black" :strokeWidth "1"}
          :label (:x-wrong warstats)
          :title "Wrong"
          :height (height-range-adjust (:x-wrong warstats))
          :transform (translate col2 centerline)]]])]))

(defn display-other-flags [targetid & {:keys [hide-inactive]}]
  (let [warstats @(rf/subscribe [:warstats-store targetid])
        flags
        [:negative-inflammatory :negative-language-warning :negative-disturbing
         :negative-logical-fallacy :negative-out-of-bounds :positive-funny
         :positive-interesting :custodial-redundant :custodial-out-of-date
         :custodial-flag-abuse :custodial-offtopic :custodial-arcane]]
    [:div [:h3 "Flags"]
     (when warstats
       (into [:div
             {:class "grid gap-1 grid-cols-2 sm:grid-cols-1 text-xs sm:text-sm"}]
            (for [flag flags
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

(defn references-summary [targetid]
  (let [references @(rf/subscribe [:references targetid])
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
      [display-other-flags rooturl]]
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
     [display-other-flags iid]]
    [references-summary iid]
    [refd-summary iid]
    [questions-and-answers iid]
    [high-scores iid]
    [controversial iid]]])
