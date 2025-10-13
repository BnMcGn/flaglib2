(ns flaglib2.hilited
  (:require
   [re-frame.alpha :as rf]
   [clojure.string :as string]

   [cljsjs.rangy-textrange]

   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]
   [flaglib2.excerpts :as excerpts]
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

(defn hilited-segment [& {:keys [text excerpt-opinions id-of-text id disable-popup? sub-opin-component]}]
  (let [popup-visible? @(rf/subscribe [:popup-is-active? id])
        db @(rf/subscribe [:core-db])
        class1 "relative font-bold"
        class2 (mood/flavor+freshness db excerpt-opinions)
        click-handler
        (fn []
          ;;Rationale: we want a popup on click unless the user is trying to select an excerpt. So
          ;; check for selection. But we want to get rid of active popup in any case of a click or
          ;; drag.
          (if (empty? (.. rangy (getSelection) (toString)))
            (rf/dispatch [:toggle-active-popup id])
            (rf/dispatch [:reset-active-popup])))
        textspan
        [:span
         {:class (str class1 " " class2)
          :style (if disable-popup?
                   {}
                   {:padding-top "0.14em" :padding-bottom "0.14em"})
          :on-click (when-not disable-popup? click-handler)}
         [segment-count (count excerpt-opinions) text]
         (excerpts/rebreak text)]]
    (if disable-popup?
      textspan
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
        :body [sub-opin-component excerpt-opinions :excerpt text :target id-of-text]]])))

(defn plain-segment [& {:keys [text]}]
  [:span {:class "font-normal"} (excerpts/rebreak text)])

;;FIXME: implement focus-parent stuff
(defn parent-segment [& {:keys [text]}]
  (let [focussed (misc/focus-parent?)
        bg (if focussed "bg-white" "bg-neutral-400")]
    [:span {:class (str "font-bold relative " bg)} (excerpts/rebreak text)]))

(defn make-segments [key
                     db
                     opids
                     & {:keys [tree-address focus root-target-url
                               disable-popup? sub-opin-component]}]
  (let [current-id (if (empty? tree-address) root-target-url (last tree-address))
        ;;FIXME: This won't handle alternate texts! Recalc-text-position counts on the text
        ;; being the one specified as proper in the database :text-store. Reengineering needed
        ;; if we ever want to display anything else!
        opins
        (for [iid opids
              :let [tpos (excerpts/recalc-text-position db iid)
                    opinion (get-in db [:opinion-store iid])]
              :when tpos]
          (if (= tpos :original)
            opinion
            (assoc opinion :text-position tpos)))
        ;;Should be pre-trimmed, but....
        text (string/trim (subs/proper-text db key))
        segpoints (excerpts/excerpt-segment-points opins (count text))
        level (count tree-address)]
    (into
     []
     (for [[start end] (partition 2 1 segpoints)
           :let [id (str "lvl-" level "-pos-" end)
                 excerpt-opinions
                 (for [opin opins
                       :let [[ostart oend] (:text-position opin)]
                       :when (excerpts/overlap? start (dec end) ostart (dec (+ ostart oend)))]
                   (:iid opin))
                 segtype (if (zero? (count excerpt-opinions))
                           plain-segment
                           (if (misc/focus? focus tree-address) hilited-segment parent-segment))]]
       [segtype
        :excerpt-opinions excerpt-opinions
        :id id
        :text (subs text start end)
        :id-of-text current-id
        :root-target-url root-target-url
        :disable-popup? disable-popup?
        :tree-address tree-address
        :focus focus
        :last-char-pos end
        :sub-opin-component sub-opin-component]))))

(defn hilited-text-core [& {:keys
                       [text-key text tree-address focus root-target-url disable-popup?
                        excerpt offset grey? sub-opin-component]}]
  (let [key (or root-target-url (last tree-address))
        text-key (or text-key key)
        text (or text @(rf/subscribe [:proper-text text-key]))
        id (str "hilited-text-" (gensym))
        coredb @(rf/subscribe [:core-db])
        opids @(rf/subscribe [:immediate-children key])
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
              :style (when grey? {:background-color "#ccc"})
              :id id
              :on-click #(.stopPropagation %)
              :on-mouse-up selection-change
              :on-key-press selection-change}]
            ;;Stray whitespace can confuse location of reply to excerpt, hence the trim
            (make-segments text-key coredb opids :tree-address tree-address :focus focus
                           :root-target-url root-target-url :disable-popup? disable-popup?
                           :sub-opin-component sub-opin-component))
      [misc/loading-indicator])))
