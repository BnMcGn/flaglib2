(ns flaglib2.grouped
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [re-com-tailwind.core :as rc]
   [clojure.string :as string]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.displayables :as disp]
   [flaglib2.titlebar :as tb]))

(defn direction-arrow-popup [direction iid]
  [:div
   [deco/casual-note-heading
    (case direction
      :pro "Link supports parent article"
      :con "Link contradicts parent article"
      :neutral "Link has undetermined effect on parent article")]
   [disp/opinion-info iid]])

(defn blank-arrow []
  [:img
   {:style {:width "18px"
            :display "inline"
            :margin-right "0.5em"}
    :src "/static/img/blank.svg"}])

(defn direction-arrow [& {:keys [iid]}]
  ;;FIXME: what if should be using rooturl warstat?
  (let [warstat @(rf/subscribe [:warstats-store iid])
        direction (when warstat (:direction-on-root warstat))
        imgsrc (when direction (str "/static/img/direction-" (name direction) ".svg"))
        popup-visible? @(rf/subscribe [:popup-is-active? iid])]
    (if direction
      [rc/popover-anchor-wrapper
       :showing? popup-visible?
       :position :below-left
       :parts {:point-wrapper {:style {:flex-flow "inherit"}}}
       :anchor
       [:img
        {:style {:width "18px"
                 :display "inline"
                 ;:height "45px"
                 :margin-right "0.5em"
                 }
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
        :body [direction-arrow-popup direction iid]]]
      [blank-arrow])))

(defn display-item-container [arrow-iid depth body & {:keys [fold]}]
  (let [small @(rf/subscribe [:window-small?])
        classes (if small "leading-8" "child:h-8")
        depth-v (if depth (nth deco/display-depths-raw depth) "0em")]
    [:div
     {:class (string/join " " ["flex items-center relative" classes])}
     [:div {:class "inline-grid justify-end self-start"
            :style {:min-width depth-v
                    :position (if fold "absolute" "relative")
                    :background-color "white"
                    :vertical-align "top"
                    :height "1.8em"} }
      (when arrow-iid [direction-arrow :iid arrow-iid])]
     body]))

(defn display-item-multiline [arrow-iid depth tbstuff & {:keys [extras]}]
  (let [classes (misc/class-string "relative" "leading-8" (:bg-color tbstuff))
        depth-v (if depth (nth deco/display-depths-raw depth) "0em")]
    [:div
     {:class classes}
     (when (and depth (not (zero? depth)))
       [:span
        {:class "inline-grid justify-end"
         :style {:min-width depth-v
                 :background-color "white"
                 :height "1.8em"
                 :vertical-align "top"
                 }}
        " "
        (if arrow-iid
          [direction-arrow :iid arrow-iid]
          [blank-arrow])])
     (into
      [:span
       {:class (:bg-color tbstuff)}
       (when-let [isize (:icon-size-mini tbstuff)]
         [:img {:src (:icon tbstuff)
                :class (misc/class-string isize "inline" "align-middle" "pl-4")}])
       (:headline tbstuff)]
      extras)]))

(defn display-item-rooturl [itm]
  (let [small @(rf/subscribe [:window-small?])
        [_ have-title _] @(rf/subscribe [:title-summary (:url itm)])
        depth (or (:display-depth itm) 0)]
    (if small
      (if have-title
        (let [db @(rf/subscribe [:core-db])
              stuff (tb/root-tb-stuff (:url itm) db)]
          [display-item-multiline
           (:refiid itm)
           depth
           stuff])
        [display-item-container
         (:refiid itm)
         (:display-depth itm)
         [disp/root-title
          :display-depth 0
          :intro-text (misc/entities "&nbsp;")
          :show-count false
          :url (:url itm)
          :hide-warstats true
          :hide-reply true
          :no-grid true
          :hide-external-link true]])
      [display-item-container
       (:refiid itm)
       (:display-depth itm)
       [disp/root-title
        :style {:width "95%"}
        :url (:url itm)
        :display-depth 0
        :hide-reply true]])))

(defn display-item-reference [itm]
  (let [small @(rf/subscribe [:window-small?])
        [_ have-title _] @(rf/subscribe [:title-summary (:reference itm)])
        depth (or (:display-depth itm) 0)]
    (if (and small have-title)
      (let [db @(rf/subscribe [:core-db])
            stuff (tb/reference-tb-stuff (:reference itm) db)]
        [display-item-multiline
         (:refopiniid itm)
         depth
         stuff])
      [display-item-container
       (:refopiniid itm)
       depth
       [disp/reference
        (:reference itm)
        :style {:width "95%"}
        :minify true
        :hide-warstats small
        :hide-external-link small]])))

(defn display-item-question [itm]
  (let [small @(rf/subscribe [:window-small?])
        depth (or (:display-depth itm) 0)]
    (if small
      (let [db @(rf/subscribe [:core-db])
            stuff (tb/question-tb-stuff (:iid itm) db)]
        [display-item-multiline
         nil
         depth
         stuff])
      [display-item-container
       nil
       depth
       [disp/question
        (:iid itm)
        :style {:width "95%"}
        :minify true
        :truncate true]])))

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
         [:dispatch [:mount-registered]]]}))
