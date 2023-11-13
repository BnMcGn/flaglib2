(ns flaglib2.grouped
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [re-com-tailwind.core :as rc]
   [clojure.string :as string]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.displayables :as disp]))

(defn direction-arrow-popup [direction iid]
  [:div
   [deco/casual-note-heading
    (case direction
      :pro "Link supports parent article"
      :con "Link contradicts parent article"
      :neutral "Link has undetermined effect on parent article")]
   [disp/opinion-info iid]])

(defn direction-arrow [& {:keys [iid]}]
  ;;FIXME: what if should be using rooturl warstat?
  (let [warstat @(rf/subscribe [:warstats-store iid])
        direction (when warstat (:direction-on-root warstat))
        imgsrc (when direction (str "/static/img/direction-" (name direction) ".svg"))
        popup-visible? @(rf/subscribe [:popup-is-active? iid])]
    (when direction
      [rc/popover-anchor-wrapper
       :showing? popup-visible?
       :position :below-left
       :parts {:point-wrapper {:style {:flex-flow "inherit"}}}
       :anchor
       [:img
        {:style {:width "18px"
                 ;:height "45px"
                 :margin-right "0.5em"}
         :src imgsrc
         ;;FIXME: Might cause problem to use iid? duplications?
         :on-click #(rf/dispatch [:toggle-active-popup iid])}]
       :popover
       [rc/popover-content-wrapper
        :parts {:border
                {:class "sm:w-[70rem]"
                 :style {:background-color "rgba(255, 255, 255, 0.7)"
                         :box-shadow "rgba(0, 0, 0, 0.3) 0px 0px 8px"
                         :border-radius "3px"}}}
        :arrow-renderer deco/wf-arrow
        :arrow-length 21
        :body [direction-arrow-popup direction iid]]])))

(defn display-item-container [arrow-iid depth body]
  (let [small @(rf/subscribe [:window-small?])
        classes (if small "leading-8" "child:h-8")
        depth-v (if depth (nth deco/display-depths-raw depth) "0em")]
    [:div
     {:class (string/join " " ["flex items-center relative" classes])}
     [:div {:class "flex justify-end self-start"
            :style {:min-width depth-v
                    :position "absolute"
                    :background-color "white"
                    :height "2.0em"} }
      (when arrow-iid [direction-arrow :iid arrow-iid])]
     body]))

(defn display-item-rooturl [itm]
  (let [small @(rf/subscribe [:window-small?])
        depth (or (:display-depth itm) 0)
        indent (nth deco/display-depths-raw depth)]
    [display-item-container
     (:refiid itm)
     (:display-depth itm)
     (if small
       [disp/root-title
        :display-depth 0
        :style {:text-indent indent}
        :class "first-line:leading-[1.8em]"
        :intro-text (misc/entities "&nbsp;")
        :show-count false
        :url (:url itm)
        :hide-warstats true
        :hide-reply true
        :no-grid true
        :hide-external-link true]
       [disp/root-title
        :style {:width "95%" :text-indent indent}
        :url (:url itm)
        :display-depth 0
        :hide-reply true])]))

(defn display-item-reference [itm]
  (let [depth (:display-depth itm)
        depth (if depth (nth deco/display-depths depth) "")]
    [:div
     {:class (str "flex items-center child:h-8 " depth)}
     [direction-arrow
      :iid (:refopiniid itm)]
     ;;FIXME: Need :warflagger-link?
     [disp/reference
      (:reference itm)
      :minify true
      :style {:width "95%"}
      :hide-reply true]]))

(defn display-item-question [itm]
  (let [depth (:display-depth itm)
        depth (if depth (nth deco/display-depths depth) "")]
    [:div
     {:class (str "flex items-center child:h-8 " depth)}
     ;;FIXME: Need :warflagger-link?
     [disp/question
      (:iid itm)
      :style {:width "95%"}
      :minify true]]))

(defn hashtags [keywords]
  (deco/casual-note-heading
   (into [:<>]
         (for [k keywords]
           [:span (str k " ")]))
  ; :style {:margin-top "2em"}
   ))

(defn display-group [group keywords]
  [:div
   {:class "child:m-[2px] mb-8"}
   (into [:<>]
     (for [itm group
           :when (:rowtype itm)
           x [(when (= 0 (:display-depth itm))
                [:div [hashtags (get keywords (:url itm))]])
              [(get {:rooturl display-item-rooturl
                     :reference display-item-reference
                     :question display-item-question}
                    (:rowtype itm))
               itm]]]
       x))])

(rf/reg-sub :grouped :-> :grouped)

(defn grouped-main []
  (let [grouped @(rf/subscribe [:grouped])]
    [:div
     ;;Rather odd stuff to match hilited
     {:on-click #(.stopPropagation %)
      :on-mouse-up #(rf/dispatch [:toggle-active-popup :parent-override])}
     [:h2 "Discussions:"]
     (when grouped
       (into [:<>]
             (for [g (:groups grouped)]
               [display-group g (:keywords grouped)])))]))

(rf/reg-event-fx
 :grouped
 (fn [{:keys [db]} _]
   {:db (assoc db :root-element grouped-main)
    :fx [ [:dispatch [:flaglib2.ipfs/request-grouped]]
          [:mount-registered db]]}))
