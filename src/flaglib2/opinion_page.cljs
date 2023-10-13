(ns flaglib2.opinion-page
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [goog.uri.utils :as uri]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.displayables :as disp]
   [flaglib2.titlebar :as tb]))


(rf/reg-fx
 :set-opinion-meta
 (fn [opinion]
   (misc/set-meta-property! "opinml:opinion" (:url opinion))
   (misc/set-meta-property! "opinml:rooturl" (:rooturl opinion))
   (misc/set-meta-property! "opinml:target" (:target opinion))))

(defn opinion-root [& {:keys [rooturl focus]}]
  (let [opinion @(rf/subscribe [:opinion-store (last focus)])]
    [:div
     {:on-click (fn [ev] (set! (. js/window -location) (misc/make-target-url rooturl)))}
     [disp/root-title
      :url rooturl
      :hide-reply true
      :intro-text "Root Target: "
      :display-depth 0]
     [disp/hilited-text
      :focus focus
      :text-key rooturl
      :root-target-url rooturl
      :tree-address '()]]))

(defn random-gray []
  (rand-nth ["#aa9" "#bbb" "#888" "#ccd" "#988" "#ddd"]))

(defn curve-locator-by-index [func max-index]
  (fn [index]
    (func (misc/relative-to-range 0 max-index index))))

;;Curve that swoops in to the left margin.
(defn layers-curve [input]
  (+ 2.0 (* 3 (Math.pow (- 1 input) 2))))

(defn opinion-layer [& {:keys [opid curve-locator focus excerpt offset]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        treead (:tree-address opinion)
        topmost (and opinion (= (count treead) (count focus)))]
    [disp/opinion-container
     {:class "absolute"
      :style {:top (str (+ 0 (* (count treead) 3)) "em")
              :left (str (curve-locator (count treead) ) "em")}}
     :box-props {:style {:border-color (if topmost "black" (random-gray))
                         :background-color (if topmost "white" "#f5f5f5")}
                 :class "bg-white border-[3px] ml-7"
                 :on-click #(set! (. js/window -location) (misc/make-opinion-url opinion))}
     :iconid opid
     :titlebar
     [:<>
      [tb/flag-name opinion]
      [tb/date-stamp opinion]
      [tb/author-long opinion]
      [tb/display-warstats :target-id opid]
      (when topmost
        [tb/reply-link :url (:url opinion) :excerpt @excerpt :offset @offset])]
     :body
     [:<>
      (when-let [comment (:comment opinion)]
        [disp/hilited-text
         :text comment
         :tree-address treead
         :focus focus])
      [disp/opinion-extras opid]]]))

(defn opinion-page []
  (let [{:keys [focus rooturl]} @(rf/subscribe [:server-parameters])
        curve-locator (curve-locator-by-index layers-curve (count focus))
        excerpt (r/atom "")
        offset (r/atom nil)]
    (fn [& {:as args}]
      (into
       [:div {:class "relative"}
        [opinion-root :rooturl rooturl :focus focus]]
       (for [opid focus]
         [opinion-layer :opid opid :curve-locator curve-locator :focus focus
          :excerpt excerpt :offset offset])))))

(rf/reg-event-fx
 :opinion-page
 (fn [{:keys [db]} _]
   ;;FIXME: Optimization: only load needed opinions from opinion tree.
   ;; -Needs a little thinking to get working: can't load subtree without opinion, rooturl, treead
   (let [rooturl (get-in db [:server-parameters :rooturl])
         focus (get-in db [:server-parameters :focus])
         db (assoc db :root-element opinion-page ;;:focus-id (last focus)
                   )]
     {:db db
      :fx [;;[:dispatch [:load-opinions focus]]
           [:dispatch [:load-rooturl rooturl]]
           [:dispatch [:flaglib2.ipfs/request-rooturl-item rooturl "opinion-tree"]]
           [:mount-registered db]]
      :set-opinion-meta {:rooturl rooturl :opinion (last focus)
                         :target (if (= 1 (count focus))
                                   rooturl
                                   (last (butlast focus)))}})))
