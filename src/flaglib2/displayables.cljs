(ns flaglib2.displayables
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [clojure.string :as string]

   [cljsjs.rangy-textrange]

   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.titlebar :as tb]
   [re-com-tailwind.core :as rc]))

(defn root-title [& {:keys [url title display-depth intro-text hide-warstats
                            warstats hide-reply hide-count reply-excerpt reply-offset
                            hide-external-link warflagger-link children]}]
  (let [warstats (or warstats @(rf/subscribe [:warstats-store url]))
        class (str (nth deco/display-depths display-depth)
                   " "
                   ;;FIXME: do we add <a> text decoration stuff here? See target-title in css
                   ((mood/flavor-from-own-warstats warstats) deco/flavor-background))]
    [rc/h-box
     :class class
     :align :center
     :children
     [intro-text
      [tb/headline :title title :rootid url :url url]
      (when (and url (not hide-external-link))
        [tb/display-external-link :url url])
      (when-not hide-warstats
        [tb/display-warstats :warstats warstats])
      children
      (when-not hide-reply
        [tb/reply-link :url url :excerpt reply-excerpt :offset reply-offset])
      (when-not hide-count
        [tb/reply-count :warstats warstats])]]))

(defn root-title-mobile [& {:keys [url title display-depth intro-text hide-warstats
                            warstats hide-reply hide-count reply-excerpt reply-offset
                            hide-external-link warflagger-link children]}]
  (let [warstats (or warstats @(rf/subscribe [:warstats-store url]))
        class (str (nth deco/display-depths display-depth)
                   " grid-cols-2 grid "
                   ;;FIXME: do we add <a> text decoration stuff here? See target-title in css
                   ((mood/flavor-from-own-warstats warstats) deco/flavor-background))]
    [:div
     {:class class}
     intro-text
     [tb/headline :title title :rootid url :url url :class "col-span-2"]
     (when (and url (not hide-external-link))
       [tb/display-external-link :url url])
     (when-not hide-warstats
       [tb/display-warstats :warstats warstats :class "justify-self-end self-center"])
     children
     (when-not hide-reply
       [tb/reply-link :url url :excerpt reply-excerpt :offset reply-offset])
     (when-not hide-count
       [tb/reply-count :warstats warstats :class "justify-self-start"])]))

(defn root-title-short [& {:as params}]
  (reduce into
   [root-title
    :show-count false
    :hide-warstats true
    :hide-reply true
    :hide-external-link true
    :intro-text ""]
   (for [[k v] params]
     [k v])))

(defn loading-indicator []
  [:div "Loading..."])

(defn opinion-info [])
(defn opinion-summary [])
(defn sub-opinion-list [])

(defn popup-side [])

(defn- find-parent-hilited [element]
  (when element
    (if (= "hilited" (. element -className))
      element
      (recur (. element -parentElement)))))

(defn is-selection-in-single-hilited-text? [selection]
  (let [parent1 (find-parent-hilited (. selection -parentElement))]
    (and (not (. selection -isCollapsed))
         parent1
         (= parent1 (find-parent-hilited (. selection -focusNode))))))

(defn segment-count [quantity]
  (when (> quantity 1)
    [:span
     {:class "absolute l-0 r-0 text-black opacity-40 text-center text-3xl top-[-1rem]"}
     quantity]))

(defn hilited-segment [& {:keys [text excerpt-opinions id-of-text]}]
  (let [warstats @(rf/subscribe [:warstats-store])
        popup-visible? (r/atom false)
        class1 "font-bold relative"
        class2 (apply mood/flavor+freshness warstats excerpt-opinions)]
    [rc/popover-anchor-wrapper
     :showing? popup-visible?
     :anchor
     [:span
      [:span
       {:class (str class1 " " class2)
        :on-click #(swap! popup-visible? not)}
       (excerpts/rebreak text)]
      [segment-count (count excerpt-opinions)]]
     :popover
     [sub-opinion-list excerpt-opinions :excerpt text :target id-of-text]]))

(defn plain-segment [& {:keys [text]}]
  [:span {:class "font-normal"} (excerpts/rebreak text)])

;;FIXME: implement focus-parent stuff
(defn parent-segment [& {:keys [text]}]
  (let [focussed (misc/focus-parent?)
        bg (if focussed "bg-white" "bg-neutral-400")]
    [:span {:class (str "font-bold relative " bg)}
     (excerpts/rebreak text)]))

(defn- make-segments [text opinion-store & {:keys [tree-address focus root-target-url hide-popup]}]
  (let [current-id (if tree-address (last tree-address) root-target-url)
        opins (misc/immediate-children-ids current-id opinion-store)
        opins (filter excerpts/has-found-excerpt? (map #(get opinion-store %) opins))
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
                   (:id opin))
                 segtype (if (zero? (count excerpt-opinions))
                           plain-segment
                           (if (misc/focus? focus tree-address) hilited-segment parent-segment))]]
       [segtype
        :excerpt-opinions excerpt-opinions
        :id id
        :text text
        :id-of-text current-id
        :root-target-url root-target-url
        :hide-popup hide-popup
        :tree-address tree-address
        :focus focus
        :last-char-pos end]))))

(defn hilited-text [& {:keys [text-key text tree-address focus root-target-url hide-popup]}]
  (let [text (or text (:text @(rf/subscribe [:text-store text-key])))
        opstore @(rf/subscribe [:opinion-store])]
    (if text
      (into [:div
             ;; :id ??
             {:class (if (misc/focus? focus tree-address) "hilited" "hilited-parent")}]
            ;;Stray whitespace can confuse location of reply to excerpt, hence the trim
            (make-segments (string/trim text) opstore :tree-address tree-address :focus focus
                           :root-target-url root-target-url :hide-popup hide-popup))
      [loading-indicator])))


(defn thread-excerpt-display
  [& {:keys [leading-context trailing-context excerpt excerpt-class]}]
  (if (or leading-context trailing-context)
    [:div
     {:class "thread-excerpt"}
     [:span (excerpts/rebreak leading-context)]
     [:span {:class excerpt-class} (excerpts/rebreak excerpt)]
     [:span (excerpts/rebreak trailing-context)]]
    [:div
     {:class "thread-excerpt thread-excerpt-unfound"}
     [:span {:class excerpt-class} (excerpts/rebreak excerpt)]]))

(defn thread-excerpt
  [& {:keys [opinion opinionid text]}]
  (let [opinion (or opinion
                    @(rf/subscribe [:opinion-store opinionid]))
        opid (or opinionid (:id opinion))
        {:keys [leading trailing excerpt]}
        (if (and opinion (:leading opinion))
          opinion
          (let [tpos (:text-position opinion)]
            (excerpts/excerpt-context text (nth tpos 0) (nth tpos 1))))]
    [thread-excerpt-display
     :leading-context leading :trailing-context trailing :excerpt excerpt
     :excerpt-class (mood/flavor+freshness @(rf/subscribe [:warstats-store nil]) opid)]))


(defn reference [])
(defn reference-default-display [])
(defn reference-excerpt-display [])
(defn question [])
(defn thread-opinion [])

(defn excerptless-opinions [])

(defn opinion-casual [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        text (if opinion
               (str "opinion by " (:authorname opinion))
               "unknown opinion")]
    [:span text]))
