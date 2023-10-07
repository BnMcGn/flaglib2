(ns flaglib2.opinion-page
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [goog.uri.utils :as uri]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.displayables :as disp]
   [flaglib2.titlebar :as tb]

   [react-helmet :as helmet]))


(defn opinion-meta [& {:keys [url rooturl target]}]
  [helmet
   [:meta :property "opinml:opinion" :content url]
   [:meta :property "opinml:rooturl" :content rooturl]
   [:meta :property "opinml:target" :content target]])

(defn opinion-root [{:keys [rooturl focus]}]
  (let [opinion @(rf/subscribe [:opinion-store (last focus)])]
    [:div
     {:on-click (fn [ev] (set! (. js/window -location) (misc/make-target-url rooturl)))}
     [disp/root-title
      :url rooturl
      :hide-reply true
      :intro-text "Root Target: "]
     [disp/hilited-text
      :focus focus
      :root-target-url rooturl
      :tree-address '()]
     [opinion-meta
      :url (:url opinion)
      :rooturl rooturl
      :target (:target opinion)]]))

(defn random-gray []
  (rand-nth ["#aa9" "#bbb" "#888" "#ccd" "#988" "#ddd"]))

(defn curve-locator-by-index [func max-index]
  (fn [index]
    (func (misc/relative-to-range 0 max-index index))))

;;Curve that swoops in to the left margin.
(defn layers-curve [input]
  (+ 5.0 (* 3 (Math.pow (- 1 input) 2))))

(defn opinion-layer [{:keys [opid curve-locator focus excerpt offset]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        treead (:tree-address opinion)
        topmost (and opinion (= (count treead) (count focus)))]
    [disp/opinion-container
     {}
     :box-props {:style {:left (str (curve-locator (count treead) "em"))
                         :border-color (if topmost "black" (random-gray))}
                 :class "bg-white border-[3px]"
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
