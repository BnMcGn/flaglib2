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

(declare reference)

(defn opinion-info [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])]
    [:div
     {:on-click #(set! (. js/window -location) (misc/make-opinion-url opinion))}
     [tb/opinion-icon opid]
     [:div
      [tb/flag-name opinion]
      [tb/date-stamp opinion]
      [tb/author-long opinion]
      [tb/display-warstats :warstats warstats]
      [:div
       (when-not (empty? (:comment opinion))
         ;;FIXME: should be clean comment?
         [:div (excerpts/rebreak (:comment opinion))])
       (when (:reference opinion)
         [reference :reference (:reference opinion)])]]]))

;;; Opinion-summary is used to display opinions in one line situations. It may be displayed with
;;; tree address icons.
(defn opinion-summary [opid & {:keys [hide-tree-address]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])]
    [:div
     (if hide-tree-address
       [tb/opinion-icon opid]
       [tb/display-tree-address (:tree-address opinion)])
     [tb/flag-name opinion]
     [tb/date-stamp opinion]
     [tb/author-long opinion]
     [tb/display-warstats :warstats warstats]
     [tb/reply-link (:url opinion)]]))

(defn sub-opinion-list [& {:keys [text tree-address root-target-url excerpt-opinions]}]
  (let [opstore @(rf/subscribe [:opinion-store])
        items
        (if (< 1 (count excerpt-opinions))
          (for [itm excerpt-opinions]
            [opinion-summary itm])
          [:opinion-info (first excerpt-opinions)])]
    (into [:div
           [:a {:href (if (empty? tree-address)
                        root-target-url
                        (get-in opstore [(last tree-address) :url]))}
            "Reply to the excerpt"]]
          items)))

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
     {:class "absolute left-0 right-0 text-black opacity-40 text-center text-4xl top-[-0.65rem]"}
     quantity]))

(rf/reg-sub
 ::popup-is-active?
 (fn [db [_ id]]
   (if (= (::active-popup db) id) true false)))

(rf/reg-event-db
 ::toggle-active-popup
 (fn [db [_ id]]
   (assoc db ::active-popup (if (= id (::active-popup db)) nil id))))

(defn hilited-segment [& {:keys [text excerpt-opinions id-of-text id]}]
  (let [warstats @(rf/subscribe [:warstats-store])
        popup-visible? @(rf/subscribe [::popup-is-active? id])
        class1 "relative font-bold"
        class2 (mood/flavor+freshness warstats excerpt-opinions)]
    [rc/popover-anchor-wrapper
     :showing? popup-visible?
     :position :below-left
     :anchor
     [:span
      [:span
       {:class (str class1 " " class2)
        :on-click #(rf/dispatch [::toggle-active-popup id])}
       [segment-count (count excerpt-opinions)]
       (excerpts/rebreak text)]
      ]
     :popover
     [sub-opinion-list excerpt-opinions :excerpt text :target id-of-text]]))

(defn plain-segment [& {:keys [text]}]
  [:span {:class "font-normal"} (excerpts/rebreak text)])

;;FIXME: implement focus-parent stuff
(defn parent-segment [& {:keys [text]}]
  (let [focussed (misc/focus-parent?)
        bg (if focussed "bg-white" "bg-neutral-400")]
    (into [:span {:class (str "font-bold relative " bg)}]
          (excerpts/rebreak text))))

(defn- make-segments [text opinion-store & {:keys [tree-address focus root-target-url hide-popup]}]
  (let [current-id (if (empty? tree-address) root-target-url (last tree-address))
        opins (if (misc/iid? current-id)
                (misc/immediate-children-ids current-id opinion-store)
                (for [[k op] opinion-store
                      :when (= current-id (:target op))]
                  k))
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
     :excerpt-class (mood/flavor+freshness @(rf/subscribe [:warstats-store nil]) [opid])]))


(defn reference-default-display [reference & {:keys [minify]}]
  (let [warstats @(rf/subscribe [:warstats-store reference])]
    [:<>
     [tb/headline
      :domain (misc/url-domain reference)
      :rootid reference
      :url (misc/make-target-url reference)]
     (when-not minify
       [tb/display-external-link :url reference :black true])
     [tb/display-warstats :warstats warstats]
     ]))

(defn reference-excerpt-display [])

(defn reference [reference & {:keys [minify]}]
  [:div
   {:class "text-white bg-black"}
   [:img {:src "/static/img/white-reference.svg"
          :class (if minify "w-[21] h-[23]" "w-[42px] h-[45px]")}]
   [(if (misc/iid? reference) reference-excerpt-display reference-default-display)
    reference]])

(defn question [])
(defn thread-opinion [])

(defn excerptless-opinions [])

(defn opinion-casual [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        text (if opinion
               (str "opinion by " (:authorname opinion))
               "unknown opinion")]
    [:span text]))
