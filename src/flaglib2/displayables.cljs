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
      [tb/headline :title title :rootid url :url true]
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
                   " grid-cols-2 grid child:justify-self-center child:self-center gap-y-0.5 pt-2 "
                   ;;FIXME: do we add <a> text decoration stuff here? See target-title in css
                   ((mood/flavor-from-own-warstats warstats) deco/flavor-background))]
    [:div
     {:class class}
     ;;intro-text
     [tb/headline :title title :rootid url :url true :class "col-span-2"]
     (when (and url (not hide-external-link))
       [tb/display-external-link :url url])
     (when-not hide-reply
       [tb/reply-link :url url :excerpt reply-excerpt :offset reply-offset])
     (when-not hide-warstats
       [tb/display-warstats :warstats warstats])
     children
     (when-not hide-count
       [tb/reply-count :warstats warstats])]))

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

(defn opinion-container [props & {:keys [iconid titlebar body box-props]
                                  :or {box-props {:class "bg-white border-[3px] border-black ml-7"}}}]
  [:div
   props
   [tb/opinion-icon iconid :style {:float "left" :position "relative" :top "3px"}]
   [:div
    box-props
    [:div {:class "flex flex-row gap-4 items-center"} titlebar]
    body]])

(defn opinion-container-mobile [props & {:keys [opinion titlebar body box-props]}]
  [:div
   (or props {})
   [tb/display-tree-address (:tree-address opinion)]
   [:div
    (merge (or box-props {})
           {:class "bg-white border-[3px] border-black"})
    [:div {:class "flex flex-row gap-4 items-center"} titlebar]
    body]])

(defn tree-address-container [props & {:keys [tree-address body fold-at]
                                       :or {fold-at 7}}]
  (let [size @(rf/subscribe [:window-size])
        fold (or (= size :xs) (>= (count tree-address) fold-at))]
    (if fold
      [:div
       props
       [tb/display-tree-address tree-address]
       body]
      [:div
       (merge
        props
        (merge {:style {:display "grid"
                        :grid-template-columns "13rem auto"
                        :grid-gap "0.25rem"}}
               (get props :style {})))
       [:div
        {:class "justify-self-end basis-52 shrink-0 grow-0"}
        [tb/display-tree-address tree-address]]
       body])))

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
(defn opinion-summary [opid & {:keys [hide-tree-address hide-icon hide-reply]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])
        size @(rf/subscribe [:window-size])]
    [:div
     {:class "flex flex-row gap-4 items-center"}
     (if hide-tree-address
       (when-not hide-icon [tb/opinion-icon opid])
       [tb/display-tree-address (:tree-address opinion)])
     (if (= size :xs)
       [tb/author-long opinion]
       [:<>
        [tb/flag-name opinion]
        [tb/date-stamp opinion]
        [tb/author-long opinion]
        [tb/display-warstats :warstats warstats]
        (when-not hide-reply [tb/reply-link (:url opinion)])])]))

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
  (let [parent1 (find-parent-hilited (. selection -anchorNode))]
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
 ::reset-active-popup
 (fn [db _]
   (assoc db ::active-popup nil)))

(defn hilited-segment [& {:keys [text excerpt-opinions id-of-text id disable-popup?]}]
  (let [warstats @(rf/subscribe [:warstats-store])
        popup-visible? @(rf/subscribe [::popup-is-active? id])
        class1 "relative font-bold"
        class2 (mood/flavor+freshness warstats excerpt-opinions)
        click-handler
        (fn [ev]
          ;;Rationale: we want a popup on click unless the user is trying to select an excerpt. So
          ;; check for selection. But we want to get rid of active popup in any case of a click or
          ;; drag.
          (if (empty? (.. rangy (getSelection) (toString)))
            (rf/dispatch [::toggle-active-popup id])
            (rf/dispatch [::reset-active-popup])))
        textspan
        [:span
         {:class (str class1 " " class2)
          :style (if disable-popup?
                   {}
                   {:padding-top "0.14em" :padding-bottom "0.14em"})
          :on-click (when-not disable-popup? click-handler)}
         [segment-count (count excerpt-opinions)]
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
        :body [sub-opinion-list excerpt-opinions :excerpt text :target id-of-text]]])))

(defn plain-segment [& {:keys [text]}]
  [:span {:class "font-normal"} (excerpts/rebreak text)])

;;FIXME: implement focus-parent stuff
(defn parent-segment [& {:keys [text]}]
  (let [focussed (misc/focus-parent?)
        bg (if focussed "bg-white" "bg-neutral-400")]
    [:span {:class (str "font-bold relative " bg)} (excerpts/rebreak text)]))

(defn- make-segments [text
                      opinion-store
                      opids
                      & {:keys [tree-address focus root-target-url disable-popup?]}]
  (let [current-id (if (empty? tree-address) root-target-url (last tree-address))
        opins (filter excerpts/has-found-excerpt? (map #(get opinion-store %) opids))
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
        :last-char-pos end]))))

(defn hilited-text [& {:keys
                       [text-key text tree-address focus root-target-url disable-popup?
                        excerpt offset grey?]}]
  (let [text (or text (:text @(rf/subscribe [:text-store text-key])))
        id (str "hilited-text-" (gensym))
        opstore @(rf/subscribe [:opinion-store])
        opids @(rf/subscribe [:immediate-children (or root-target-url (last tree-address))])
        selection-change
        (fn [ev]
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
                (rf/dispatch [::toggle-active-popup :parent-override])
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
            (make-segments (string/trim text) opstore opids :tree-address tree-address :focus focus
                           :root-target-url root-target-url :disable-popup? disable-popup?))
      [loading-indicator])))


(defn thread-excerpt-display
  [& {:keys [leading-context trailing-context excerpt excerpt-class]}]
  (let [icon-style {:width "42px" :height "45px" :float "left" :top "-1em" :margin-right "1em"}]
    [:div
     {:class "thread-excerpt italic text-sm mt-2 mb-4 sm:mr-40 mr-6 min-h-[3em] ml-6 sm:break-normal break-all sm:break-words"}
     [:img
      (if (or leading-context trailing-context)
        {:src "/static/img/black-wf-quote.svg" :style icon-style}
        {:src "/static/img/red-wf-quote.svg"
         :style icon-style
         :title "Excerpt Not Found"})]
     [:span {:style {:background-color "#eee"}}
      [:span (excerpts/rebreak leading-context)]
      [:span {:class excerpt-class} (excerpts/rebreak excerpt)]
      [:span (excerpts/rebreak trailing-context)]]]))

(defn thread-excerpt
  [& {:keys [opinion opinionid text]}]
  (let [opinion (or opinion
                    @(rf/subscribe [:opinion-store opinionid]))
        opid (or opinionid (:id opinion))
        {:keys [leading trailing excerpt]}
        (cond (and opinion (:leading opinion)) opinion
              text (let [tpos (:text-position opinion)]
                     (excerpts/excerpt-context text (nth tpos 0) (nth tpos 1)))
              :else opinion)]
    [thread-excerpt-display
     :leading-context leading :trailing-context trailing :excerpt excerpt
     :excerpt-class (mood/flavor+freshness @(rf/subscribe [:warstats-store nil]) [opid])]))


(defn reference-root-display [reference & {:keys [minify]}]
  (let [warstats @(rf/subscribe [:warstats-store reference])]
    [:<>
     [tb/headline
      :domain (misc/url-domain reference)
      :rootid reference
      :url true]
     (when-not minify
       [tb/display-external-link :url reference :black true])
     [tb/display-warstats :warstats warstats :black true]]))

(defn reference-excerpt-display [])

(defn reference [reference & {:keys [minify]}]
  [:div
   {:class "text-white bg-black flex flex-row items-center gap-4 pl-2 pb-0.5"}
   [:img {:src "/static/img/white-reference.svg"
          :class (if minify "min-w-[21] h-[23]" "min-w-[42px] h-[45px]")}]
   [(if (misc/iid? reference) reference-excerpt-display reference-root-display)
    reference]])

;;Various ways to describe incoming references. Viewing the root article. Need to cover refs to root,
;; refs to excerpt of root, and refs to opinions in discussion of root.
(defn display-refd-root-pov [refopid & {:keys [minify]}]
  (let [refop @(rf/subscribe [:opinion-store refopid])
        from-domain (misc/url-domain (:rooturl refop))
        refdop (when-let [refd (:refd refop)] @(rf/subscribe [:opinion-store refd]))
        ref-to-root-excerpt? (and refdop (excerpts/qualifies-as-excerpt-marker? refdop)
                                  (not (misc/deep-opinion? refdop)))]
    [:div
     {:class "text-white bg-black flex flex-row items-center gap-4 pl-2 pb-0.5"
      :style {:background-color (deco/stripes-45 "transparent" "#fff7")
              :background-size "20px 20px"}}
     [:a {:href (misc/make-opinion-url refop)}
      [:img {:src "/static/img/white-reference.svg"
             :class (if minify "min-w-[21] h-[23]" "min-w-[42px] h-[45px]")}]]
     (cond
       ref-to-root-excerpt?
       [:<> (str "From " from-domain " To Excerpt:" )
        [:a {:class "italic" :href (misc/make-opinion-url refop)} (:excerpt refdop)]]
       refdop
       [:<> (str "From " from-domain " To: ") [tb/display-tree-address (:tree-address refdop)]]
       :else
       (str "From " from-domain))]))

(defn question-container [props & {:keys [body minify]}]
  [:div
   (merge {:class "flex flex-row items-center bg-[#f5eb72] pl-1.5 gap-4"} props)
   [:img {:src "/static/img/black-wf-question.svg"
          :class (if minify "min-w-[21] h-[23]" "max-w-[42px] h-[45px]")}]
   body])

(defn question [opid & {:keys [minify]}]
  (when-let [opinion @(rf/subscribe [:opinion-store opid])]
    [question-container
     {}
     :minify minify
     :body
     [:<>
      [:a {:href (misc/make-opinion-url opinion)}
       ;;FIXME: Should manually truncate?
       [tb/comment-summary :opinion opinion :truncate minify]]
      [tb/display-warstats :warstats @(rf/subscribe [:warstats-store opid])]]]))

(defn opinion-extras [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])]
    [:div
     {:class "sm:flex sm:flex-row child:sm:grow child:sm:basis-0"}
     (when-let [ref (:reference opinion)] [reference ref])
     (when (:question warstats) [question-container {}])]))

(defn thread-opinion [& {:keys [opid text]}]
  (let [excerpt (r/atom "")
        offset (r/atom nil)]
    (fn [& {:as args}]
      (let
          [opinion @(rf/subscribe [:opinion-store opid])
           warstats @(rf/subscribe [:warstats-store opid])]
        (when opinion
          (let [tree-address (:tree-address opinion)
                parid (when (< 1 (count tree-address))
                        (nth tree-address (- (count tree-address) 2)))
                parent (when parid
                         @(rf/subscribe [:opinion-store parid]))
                text (if parent
                       (or (:comment parent) "")
                       text)
                small  (= :xs @(rf/subscribe [:window-size]))
                tbar (if small
                       [:<>
                        [tb/author-long opinion]
                        [tb/display-warstats :warstats warstats]]
                       [:<>
                        [tb/flag-name opinion]
                        [tb/date-stamp opinion]
                        [tb/author-long opinion]
                        [tb/display-warstats :warstats warstats]
                        ;;FIXME: should handle excerpts, could use iid instead of url?
                        #_[tb/reply-link :url (:url opinion) :excerpt @excerpt :offset @offset]])
                main-style
                (if
                  small
                  {:width "100%"}
                  {:margin-left (deco/thread-opinion-indent (dec (count tree-address))) :width "80%"})]
            [(if small opinion-container-mobile opinion-container)
             {:class "mb-6 sm:break-normal break-all sm:break-words"
              :style main-style
              :on-click (fn [e]
                          (set! (. js/window -location) (misc/make-opinion-url opinion))
                          (.stopPropagation e))}
             :iconid opid
             :opinion opinion
             :titlebar tbar
             :body
             [:<>
              [:div
               {:class "m-4 mt-1"}
               ;; {:overflow "overlay"} ??
               (when (excerpts/has-excerpt? opinion)
                 [thread-excerpt :opinionid opid :text text])
               (when (:comment opinion)
                 [hilited-text
                  :text (:comment opinion)
                  :tree-address tree-address
                  :focus nil
                  :disable-popup? true
                  :excerpt excerpt
                  :offset offset])]
              [opinion-extras opid]]]))))))

(defn excerptless-opinions [target-id]
  (let [opstore @(rf/subscribe [:opinion-store])
        idlist @(rf/subscribe [:immediate-children target-id])
        idlist (remove #(excerpts/has-excerpt? (get opstore %1)) idlist)]
    (when-not (empty? idlist)
      [:div
       {:class "mt-4"}
       [:h3 "Replies:"]
       (into [:div] (map #(vector thread-opinion :opid %1) idlist))])))

(defn opinion-casual [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        text (if opinion
               (str "opinion by " (:authorname opinion))
               "unknown opinion")]
    [:span text]))
