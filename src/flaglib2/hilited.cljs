(ns flaglib2.hilited
  (:require
   [re-frame.alpha :as rf]
   [re-frame.db]
   [clojure.string :as string]

   [cljsjs.rangy-textrange]

   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.visibility :as vis]
   [flaglib2.subscriptions :as subs]
   [re-com-tailwind.core :as rc]))

(def rangy js/rangy)

(defn popup-side [])

(defn- find-parent-hilited [element]
  (when element
    (if (= "hilited" (. element -className))
      element
      (recur (. element -parentElement)))))

(defn is-selection-in-single-hilited-text? [selection]
  (let [parent1 (find-parent-hilited (. selection -anchorNode))]
    (and (not (. selection -isCollapsed))
         parent1
         (= parent1 (find-parent-hilited (. selection -focusNode))))))

(defn segment-count [quantity text]
  (when (and (> quantity 1)
             (cond ;; If text is too short to cover digits it will make a mess. Don't display.
               (> quantity 999) (> (count text) 9)
               (> quantity 99) (> (count text) 6)
               (> quantity 9) (> (count text) 4)
               :else (> (count text) 2))
             (not (every? misc/whitespace-characters text)))
    [:span
     {:class "absolute left-0 right-0 text-black opacity-40 text-center text-4xl top-[-0.45rem]"}
     quantity]))

(rf/reg-sub
 :popup-is-active?
 (fn [db [_ id]]
   (if (= (::active-popup db) id) true false)))

(rf/reg-event-db
 :toggle-active-popup
 (fn [db [_ id]]
   (let [active (::active-popup db)]
     (assoc db ::active-popup
            ;;Due to not figuring out the stopPropagation thing:
            ;; click on hilite when popup active will cause a double cancel, resulting in
            ;; popup not going away. So parent sends :parent-override, which we handle by wrapping
            ;; existing id in a vector to deactivate.
            (if (= id :parent-override)
              (if (vector? active)
                nil
                (if active
                  [active]
                  nil))
              (if (vector? active)
                (if (= [id] active)
                  nil
                  id)
                (if (= id active)
                  nil
                  id)))))))

(rf/reg-event-db
 :reset-active-popup
 (fn [db _]
   (assoc db ::active-popup nil)))

(defn- hilited-segment-popover [& {:keys [id textspan popup-body]}]
  (let [popup-visible? @(rf/subscribe [:popup-is-active? id])]
    [rc/popover-anchor-wrapper
     :showing? popup-visible?
     :position :below-left
     :style {:display "inline"}

     :parts {:point-wrapper {:style {:display "inline"}}}
     :anchor
     textspan
     :popover
     [rc/popover-content-wrapper
      :parts {:border
              {:class "sm:w-[70rem]"
               :style {:background-color "rgba(255, 255, 255, 0.7)"
                       :box-shadow "rgba(0, 0, 0, 0.3) 0px 0px 8px"
                       :border-radius "3px"}}}
      :arrow-renderer deco/wf-arrow
      :arrow-length 21
      :body popup-body]]))

(defn hilited-segment [key index & {:keys [disable-popup? sub-opin-component]}]
  (let [{:keys [segment-id excerpt-opinions id-of-text text tree-address warn-off?
                warn-offs start end]}
        @(rf/subscribe [:segments-segment key index])
        db @(rf/subscribe [:core-db])
        stylespec (if disable-popup?
                    {}
                    {:padding-top "0.14em" :padding-bottom "0.14em"})
        click-handler
        (fn []
          ;;Rationale: we want a popup on click unless the user is trying to select an excerpt. So
          ;; check for selection. But we want to get rid of active popup in any case of a click or
          ;; drag.
          (if (empty? (.. rangy (getSelection) (toString)))
            (rf/dispatch [:toggle-active-popup segment-id])
            (rf/dispatch [:reset-active-popup])))
        textspan
        (if warn-off?
          [:span
           {:style (merge stylespec (vis/warn-off-small-style (first (first warn-offs))))
            :on-click (when-not disable-popup? click-handler)}
           [:span {:style {:visibility "hidden"}} (excerpts/rebreak text)]]
          [:span
           {:class (str "relative font-bold " (mood/flavor+freshness db excerpt-opinions))
            :style stylespec
            :on-click (when-not disable-popup? click-handler)}
           [segment-count (count excerpt-opinions) text]
           (excerpts/rebreak text)])]
    (if disable-popup?
      textspan
      [hilited-segment-popover
       :id segment-id
       :textspan textspan
       :popup-body
       [sub-opin-component excerpt-opinions
        :excerpt text :target id-of-text :warn-off? warn-off?]])))

(defn plain-segment [key index]
  (let [{:keys [text]} @(rf/subscribe [:segments-segment key index])]
    [:span {:class "font-normal"} (excerpts/rebreak text)]))

;;FIXME: implement focus-parent stuff
(defn parent-segment [key index]
  (let [{:keys [text]} @(rf/subscribe [:segments-segment key index])
        focussed (misc/focus-parent?)
        bg (if focussed "bg-white" "bg-neutral-400")]
    [:span {:class (str "font-bold relative " bg)} (excerpts/rebreak text)]))

(defn make-segments [key & {:keys [focus root-target-url disable-popup? sub-opin-component]}]
  (into
   []
   (map-indexed
    (fn [index seg]
      [(case (:segment-type seg)
         :plain plain-segment
         :hilited (if (misc/focus? focus (:tree-address seg))
                    hilited-segment parent-segment))
       key
       index
       :root-target-url root-target-url ;Need this?
       :disable-popup? disable-popup?
       :focus focus
       :sub-opin-component sub-opin-component])
    @(rf/subscribe [:segments key]))))

(defn hilited-text-core [& {:keys
                       [text-key text tree-address focus root-target-url disable-popup?
                        excerpt offset grey? sub-opin-component hidden]}]
  (let [key (or root-target-url (last tree-address))
        text-key (or text-key key)
        text (or text @(rf/subscribe [:proper-text text-key]))
        id (str "hilited-text-" (gensym))
        selection-change
        (fn []
          (when (and excerpt offset)
            (if (is-selection-in-single-hilited-text? (. rangy (getSelection)))
              (let [textel (. js/document (getElementById id))
                    range (.. rangy (getSelection) (getRangeAt 0))
                    loc (excerpts/text-location-from-dom-range textel range)
                    ex (excerpts/get-location-excerpt
                        (excerpts/create-textdata (string/trim text))
                        (:start loc) (:end loc))]
                (reset! excerpt (:excerpt ex))
                (reset! offset (:offset ex)))
              (do
                (rf/dispatch [:toggle-active-popup :parent-override])
                (reset! excerpt "")
                (reset! offset nil)))))]
    (if text
      (into [:div
             {:class "hilited" ;Don't remove. Needed for finding selection.
              :style (cond-> {}
                       grey? (assoc :background-color "#ccc")
                       hidden (assoc :visibility "hidden"))
              :id id
              :on-click #(.stopPropagation %)
              :on-mouse-up selection-change
              :on-key-press selection-change}]
            ;;Stray whitespace can confuse location of reply to excerpt, hence the trim
            (make-segments text-key :focus focus
                           :root-target-url root-target-url :disable-popup? disable-popup?
                           :sub-opin-component sub-opin-component))
      [misc/loading-indicator])))
