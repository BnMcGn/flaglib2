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

(def rangy js/rangy)

(defn root-title-fullscreen [& {:keys [url title display-depth intro-text hide-warstats
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
     [(when intro-text [:span {:class "font-bold"} intro-text])
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

(defn root-title [& {:as args}]
  (let [size @(rf/subscribe [:window-size])
        rt (if (= size :xs) root-title-mobile root-title-fullscreen)]
    (reduce into [rt] (seq args))))

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

(defn opinion-container [props & {:keys [iconid titlebar body]}]
  [:div
   props
   [tb/opinion-icon iconid :float-left true]
   [:div
    {:class "bg-white border-[3px] border-black ml-7"}
    [:div {:class "flex flex-row gap-4 items-center"} titlebar]
    body]])

(defn opinion-info [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])
        size @(rf/subscribe [:window-size])]
    [opinion-container
     {:on-click #(set! (. js/window -location) (misc/make-opinion-url opinion))}
     :iconid opid
     :titlebar
     (if (= size :xs)
       [tb/author-long opinion]
       [:<>
        [tb/flag-name opinion]
        [tb/date-stamp opinion]
        [tb/author-long opinion]
        [tb/display-warstats :warstats warstats]])
     :body
     [:div
      (when-not (empty? (:comment opinion))
        ;;FIXME: should be clean comment?
        [:div (excerpts/rebreak (:comment opinion))])
      (when (:reference opinion)
        [reference :reference (:reference opinion)])]]))

;;; Opinion-summary is used to display opinions in one line situations. It may be displayed with
;;; tree address icons.
;;FIXME: Would be nice to have an icon that indicates presence of a comment
(defn opinion-summary [opid & {:keys [hide-tree-address]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])
        size @(rf/subscribe [:window-size])]
    [:div
     {:class "flex flex-row gap-4 items-center"}
     (if hide-tree-address
       [tb/opinion-icon opid]
       [tb/display-tree-address (:tree-address opinion)])
     (if (= size :xs)
       [tb/author-long opinion]
       [:<>
        [tb/flag-name opinion]
        [tb/date-stamp opinion]
        [tb/author-long opinion]
        [tb/display-warstats :warstats warstats]
        [tb/reply-link (:url opinion)]])]))

(defn sub-opinion-list [excerpt-opinions
                        & {:keys [excerpt tree-address root-target-url]}]
  (let [opstore @(rf/subscribe [:opinion-store])
        items
        (if (< 1 (count excerpt-opinions))
          (for [itm excerpt-opinions]
            [opinion-summary itm])
          [[opinion-info (first excerpt-opinions)]])]
    (into [:div
           ;;FIXME: what about offset?
           [:div
            {:class "mb-2"}
            [:a {:href (misc/excerpt-reply-link
                        (if (empty? tree-address)
                          root-target-url
                          (get-in opstore [(last tree-address) :url]))
                        excerpt)
                 :style {:color "black"}
                 :class "m-0 bold italic bg-gray-300 leading-4"}
             "Reply to the excerpt"]]]
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
       (excerpts/rebreak text)]]
     :popover
     [rc/popover-content-wrapper
      :parts {:border
              {:class "sm:w-[70rem]"
               :style {:background-color "rgba(255, 255, 255, 0.7)"
                       :box-shadow "rgba(0, 0, 0, 0.3) 0px 0px 8px"
                       :border-radius "3px"}}}
      :arrow-renderer deco/wf-arrow
      :arrow-length 21
      :body [sub-opinion-list excerpt-opinions :excerpt text :target id-of-text]]]))

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

(defn hilited-text [& {:keys [text-key text tree-address focus root-target-url hide-popup excerpt offset]}]
  (let [text (or text (:text @(rf/subscribe [:text-store text-key])))
        opstore @(rf/subscribe [:opinion-store])
        selection-change
        (fn [ev]
          (this-as this
            (when (and excerpt offset)
             (if (is-selection-in-single-hilited-text? (. rangy (getSelection)))
               (let [textel this
                     range (.. rangy (getSelection) (getRangeAt 0) (toCharacterRange textel))
                     ex (excerpts/get-location-excerpt
                         (excerpts/create-textdata (string/trim text))
                         (. range -start) (. range -end))]
                 (reset! excerpt (:excerpt ex))
                 (reset! offset (:offset ex)))
               (do
                 (reset! excerpt "")
                 (reset! offset nil))))))]
    (if text
      (into [:div
             ;; :id ??
             {:class (if (misc/focus? focus tree-address) "hilited" "hilited-parent")
              :on-click #(.stopPropagation %)
              :on-mouse-up selection-change
              :on-key-press selection-change}]
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

(defn thread-opinion [& {:keys [opid text]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])
        excerpt (r/atom "")
        offset (r/atom nil)]
    (when opinion
      (let [tree-address (:tree-address opinion)
            parid (when (< 1 (count tree-address))
                    (nth tree-address (- (count tree-address) 2)))
            parent (when parid
                     @(rf/subscribe [:opinion-store parid]))
            text (if parent
                   (or (:comment parent) "")
                   text)]
        [opinion-container
         {} ;; Depth stuff
         :iconid opid
         :titlebar
         [:<>
          [tb/flag-name opinion]
          [tb/date-stamp opinion]
          [tb/author-long opinion]
          [tb/display-warstats :warstats warstats]
          ;;FIXME: should handle excerpts, could use iid instead of url?
          [tb/reply-link :url (:url opinion) :excerpt @excerpt :offset @offset]]
         :body
         [:div
          ;; {:overflow "overlay"} ??
          (when (excerpts/has-excerpt? opinion)
            [thread-excerpt :opinionid opid :text text])
          (when (:comment opinion)
            [hilited-text
             :text (:comment opinion)
             :tree-address tree-address
             :hide-popup true
             :excerpt excerpt
             :offset offset])
          [:div
           ;; ref and question
           ]]]))))

(defn excerptless-opinions [])

(defn opinion-casual [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        text (if opinion
               (str "opinion by " (:authorname opinion))
               "unknown opinion")]
    [:span text]))
