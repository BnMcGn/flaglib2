(ns flaglib2.opinion-page
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [goog.uri.utils :as uri]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.displayables :as disp]
   [flaglib2.titlebar :as tb]
   [flaglib2.target-summary :as tsum]
   [flaglib2.hixer :as-alias hixer]
   [flaglib2.hixer :as hixer]))

(defn opinion-root [& {:keys [rooturl focus]}]
  (let [small @(rf/subscribe [:window-small?])]
    [:div
     {:on-click (fn [] (set! (. js/window -location) (misc/make-target-url rooturl)))}
     [disp/root-title
      :url rooturl
      :hide-reply true
      :hide-count small
      :hide-external-link small
      :hide-warstats small
      :intro-text "Root Target: "
      :display-depth 0]
     [disp/hilited-text
      :focus focus
      :text-key rooturl
      :grey? true
      :root-target-url rooturl
      :tree-address '()]]))

(defn random-gray []
  (rand-nth ["#aa9" "#bbb" "#888" "#ccd" "#988" "#ddd"]))

(defn curve-locator-by-index [func max-index]
  (fn [index]
    (func (misc/relative-to-range 0 max-index index))))

;;Curve that swoops in to the left margin.
(defn layers-curve [input]
  (+ 0.75 (* 3 (js/Math.pow (- 1 input) 2))))

(defn opinion-layer [& {:keys [opid curve-locator focus excerpt offset]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        treead (:tree-address opinion)
        topmost (and opinion (= (count treead) (count focus)))
        small @(rf/subscribe [:window-small?])
        titlebar
        [:<>
         [tb/flag-name opinion]
         [tb/date-stamp opinion]
         [tb/author-long opinion]
         [tb/display-warstats :target-id opid]
         (when topmost
           [tb/reply-link :target (:iid opinion) :excerpt @excerpt :offset @offset])]
        box-props
        ;;FIXME; Hack to make click on reference block work. Should be a better way to get clicks thru.
        (into (if topmost {}
                  {:on-click #(set! (. js/window -location) (misc/make-opinion-url opinion))})
              {:style {:border-color (if topmost "black" (random-gray))
                       :background-color (if topmost "white" "#f5f5f5")}
               :class "bg-white border-[3px] ml-7"})
        body
        [:<>
         [:div
          {:class "m-4 mt-1"}
          (when-let [comment (:clean-comment opinion)]
            [disp/hilited-text
             :text comment
             :tree-address treead
             :focus focus
             :excerpt excerpt
             :offset offset])
          [disp/opinion-extras opid]
          (when topmost
            [disp/excerptless-opinions opid])]]
        class  "absolute sm:break-normal break-all sm:break-words"]
    (when opinion
      (if small
        (let [hpos (- (curve-locator (count treead)) 0.75)]
          [disp/opinion-container-mobile
           {:class class
            :style {:top (str (+ -1.3 (* (count treead) 2.5)) "em")
                    :left (str hpos "em")
                    :width (str "calc(100% - " hpos "em)")}}
           :box-props box-props
           :icon-layout :icon
           :opinion opinion
           :titlebar titlebar
           :body body])
        [disp/opinion-container
         {:class class
          :style {:top (str (+ -0.5 (* (count treead) 3)) "em")
                  :left (str (curve-locator (count treead)) "em")
                  :width "75%"}}
         :box-props box-props
         :iconid opid
         :titlebar titlebar
         :body body]))))

(defn opinion-thread []
  (let [{:keys [focus rooturl]} @(rf/subscribe [:server-parameters])
        curve-locator (curve-locator-by-index layers-curve (count focus))
        excerpt (r/atom "")
        offset (r/atom nil)]
    (fn [& _]
      (into
       [:div {:class "relative"}
        [opinion-root :rooturl rooturl :focus focus]]
       (for [opid focus]
         [opinion-layer :opid opid :curve-locator curve-locator :focus focus
          :excerpt excerpt :offset offset])))))


(defn opinion-title-thread [& {:keys [iid]}]
  (let [title-tree @(rf/subscribe [:title-tree iid])
        db @(rf/subscribe [:core-db])]
    [:div
     [disp/opinion-summary iid :hide-tree-address true :tt true]
     (when-not (empty? title-tree)
       (into [:<> [:h3 "Title discussion:"]]
             (map (fn [opid]
                      (let [opinion (get-in db [:opinion-store opid])
                            indicate (and (not (misc/deep-opinion? opinion))
                                          (misc/opinion-suggests-tt? opinion))
                            subs (when indicate
                                   {:opinion-icon
                                    [tb/opinion-icon-tt
                                     :description "title"
                                     :supply? (misc/opinion-supplies-title? opinion db)]})]
                        [disp/thread-opinion :opid opid :substitutes subs]))
                    (flatten title-tree))))]))

(defn opinion-page []
  (let [{:keys [tmode focus]} @(rf/subscribe [:server-parameters])
        has-hiccup? @(rf/subscribe [:hiccup-store (last focus)])
        tmode (keyword tmode)
        tmode (if has-hiccup?
                tmode
                (if (= tmode :article) nil tmode))
        current (r/atom (if tmode tmode :thread))
        tabs [(when has-hiccup? {:id :article :label "Article View"})
              {:id :thread :label "Thread View"}
              {:id :summary :label "Summary"}
              {:id :title :label "Title"}]
        tabs (into [] (filter #'identity tabs))
        small @(rf/subscribe [:window-small?])]
    [:<> [(if small rc/vertical-bar-tabs rc/horizontal-tabs)
          :model current
          :tabs tabs
          :parts {:wrapper {:class "mt-2"}
                  :anchor {:style {:color "#777"}}}
          :on-change #(set! js/window.location.href
                            (uri/setParam js/window.location.href "tmode" (name %1)))]
     (case @current
       :article [hixer/opinion-hiccup (last focus)]
       :thread [opinion-thread]
       :summary [tsum/opinion-stats :iid (last focus)]
       :title [opinion-title-thread :iid (last focus)])]))

(rf/reg-event-fx
 :opinion-page
 (fn [{:keys [db]} _]
   ;;FIXME: Optimization: only load needed opinions from opinion tree.
   ;; -Needs a little thinking to get working: can't load subtree without opinion, rooturl, treead
   (let [rooturl (get-in db [:server-parameters :default :rooturl])
         focus (get-in db [:server-parameters :default :focus])
         db (assoc db :root-element opinion-page ;;:focus-id (last focus)
                   )]
     {:db db
      :fx [;;[:dispatch [:load-opinions focus]]
           [:dispatch [:load-rooturl rooturl]]
           [:dispatch [:flaglib2.hixer/request-opinion-hiccup (last focus)]]
           [:dispatch [:flaglib2.ipfs/request-rooturl-item rooturl "opinion-tree"]]
           [:dispatch [:mount-registered]]]
      :set-opinion-meta {:rooturl rooturl :opinion (last focus)
                         :target (if (= 1 (count focus))
                                   rooturl
                                   (last (butlast focus)))}})))
