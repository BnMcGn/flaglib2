(ns flaglib2.displayables
  (:require
   [re-frame.alpha :as rf]
   [reagent.core :as r]
   [clojure.set :as set]
   [clojure.string :as string]

   [cljsjs.rangy-textrange]

   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]
   [flaglib2.flags :as flags]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.hilited :as hi]
   [flaglib2.titlebar :as tb]
   [flaglib2.visibility :as vis]
   [re-com-tailwind.core :as rc]))

(def rangy js/rangy)


(defn root-title [& {:keys [style url display-depth warstats no-grid class
                            intro-text title headline-style headline-class
                            reply-excerpt reply-offset children tt
                            hide-reply hide-warstats hide-count hide-external-link
                            no-main-link]}]
  (let [intro (when intro-text [:span {:class "font-bold"} intro-text])
        small @(rf/subscribe [:window-small?])
        dd (if (= false display-depth) ""
               (nth deco/display-depths (or display-depth 0)))
        warstats (or warstats @(rf/subscribe [:warstats-store url]))
        db @(rf/subscribe [:core-db])
        tbstuff (tb/root-tb-stuff
                 url db
                 :reply-excerpt reply-excerpt
                 :reply-offset reply-offset
                 :warstats warstats)
        box (if no-grid
              ""
              (if small
                "grid-cols-2 grid child:justify-self-center child:self-center gap-y-0.5 pt-2"
                "flex flex-row items-center"))
        class (misc/class-string dd (:bg-color tbstuff) box class)
        props {:class class :style style}
        head (into (:headline tbstuff)
                   [:style headline-style :class headline-class
                    :url (if no-main-link false true)])
        head (if title (into head [:title title]) head)
        link (when (and url (not hide-external-link)) (:external-link tbstuff))
        rep (when-not hide-reply (:reply-link tbstuff))
        rep (if (and rep tt) (into [tb/reply-link-tt] (rest rep)) rep)
        ws (when-not hide-warstats (:warstats tbstuff))
        ct (when-not hide-count (:count tbstuff))]
    (into [:div props intro]
          (if small
            [head link rep ws children ct]
            [head link ws children rep ct]))))

(declare reference)
(declare thread-excerpt)

(defn opinion-container [props & {:keys [iconid titlebar body box-props substitutes]
                                  :or {box-props {:class "bg-white border-[3px] border-black ml-7"}}}]
  [:div
   props
   (tb/rewidget-item
    [tb/opinion-icon
     iconid
     :style {:float "left" :position "relative" :top "3px"}]
    (:opinion-icon substitutes))
   [:div
    box-props
    [:div {:class "flex flex-row gap-4 items-center"} titlebar]
    body]])

(defn opinion-container-mobile [props & {:keys [opinion titlebar body box-props icon-layout substitutes]
                                         :or {icon-layout :tree-address}}]
  [:div
   (or props {})
   (case icon-layout
     :tree-address [tb/display-tree-address (:tree-address opinion) :substitutes substitutes]
     :icon (tb/rewidget-item [tb/opinion-icon (:iid opinion)] (:opinion-icon substitutes))
     nil)
   [:div
    (merge (or box-props {})
           {:class "bg-white border-[3px] border-black mt-1"})
    [:div {:class "grid-cols-2 grid child:justify-self-center child:self-center gap-y-0.5"} titlebar]
    body]])

;;FIXME: refactor -> *-tb-stuff ??
;;Displays right justified tree address beside or above body
(defn tree-address-container [props & {:keys [tree-address body fold-at]
                                       :or {fold-at 7}}]
  (let [small @(rf/subscribe [:window-small?])
        fold (or small (>= (count tree-address) fold-at))]
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

(defn opinion-info [opid & {:keys [show-excerpt]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])
        small @(rf/subscribe [:window-small?])]
    [opinion-container
     {:on-click #(set! (. js/window -location) (misc/make-opinion-url opinion))}
     :iconid opid
     :titlebar
     (if small
       [tb/author-long opinion]
       [:<>
        [tb/flag-name opinion]
        [tb/date-stamp opinion]
        [tb/author-long opinion]
        [tb/display-warstats :warstats warstats]])
     :body
     [:div
      (when (and show-excerpt (:excerpt opinion))
        [thread-excerpt :opinionid opid])
      (when-not (empty? (:clean-comment opinion))
        [:div (excerpts/rebreak (:clean-comment opinion))])
      (when (:reference opinion)
        [reference opinion])]]))

;;; Opinion-summary is used to display opinions in one line situations. It may be displayed with
;;; tree address icons.
;;FIXME: Would be nice to have an icon that indicates presence of a comment
(defn opinion-summary [opid & {:keys [hide-tree-address hide-icon hide-reply tt]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])
        small @(rf/subscribe [:window-small?])]
    [:div
     {:class "flex flex-row gap-4 items-center"}
     (if hide-tree-address
       (when-not hide-icon [tb/opinion-icon opid])
       [tb/display-tree-address (:tree-address opinion)])
     (if small
       [tb/author-long opinion]
       [:<>
        [tb/flag-name opinion]
        [tb/date-stamp opinion]
        [tb/author-long opinion]
        [tb/display-warstats :warstats warstats]
        (when-not hide-reply [(if tt tb/reply-link-tt tb/reply-link)
                              :target (:iid opinion)
                              :hide-text true])])]))

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
                          (last tree-address))
                        excerpt)
                 :style {:color "black"}
                 :class "m-0 bold italic bg-gray-300 leading-4"}
             "Reply to the excerpt"]]]
          items)))

(defn hilited-text [& {:keys
                       [text-key text tree-address focus root-target-url disable-popup?
                        excerpt offset grey? hidden]}]
  [hi/hilited-text-core
   :text-key text-key
   :text text
   :tree-address tree-address
   :focus focus
   :root-target-url root-target-url
   :disable-popup? disable-popup?
   :excerpt excerpt
   :offset offset
   :grey? grey?
   :hidden hidden
   :sub-opin-component sub-opinion-list])

(defn quote-icon [& {:keys [status style]}]
  (let [[src title]
        (case status
          :not-found
          ["/static/img/red-wf-quote.svg" "Excerpt Not Found"]
          :warn-off
          ["/static/img/red-wf-quote.svg" "Target Article Restricted"]
          ["/static/img/black-wf-quote.svg" nil])]
    [:img {:style style :src src :title title}]))

(defn excerpt-spans [chunks]
  (into
   [:<>]
   (for [[type text] chunks]
     (cond
       (string? type) [:span {:class type} (excerpts/rebreak text)]
       (= :normal type) [:span (excerpts/rebreak text)]
       (= :warn-off type)
       ;;Whatever flag...
       [:span {:style (vis/warn-off-small-style :negative-disturbing)}
        [:span {:style {:visibility "hidden"}} (excerpts/rebreak text)]]))))

(defn thread-excerpt-display [& {:keys [chunks status]}]
  (let [icon-style {:width "42px" :height "45px" :float "left" :top "-1em"
                    :margin-right "1em"}]
    [:div
     {:class "thread-excerpt italic text-sm mt-2 mb-4 sm:mr-40 mr-6 min-h-[3em] ml-6 sm:break-normal break-all sm:break-words"}
     [quote-icon :status status :style icon-style]
     [excerpt-spans chunks]]))

(defn thread-excerpt [& {:keys [opinion opinionid text]}]
  (let [opinion (or opinion @(rf/subscribe [:opinion-store opinionid]))
        opid (or opinionid (:iid opinion))
        cinfo @(rf/subscribe [:excerpt-context-info opid])]
    (cond
      (= :not-found cinfo)
      [thread-excerpt-display
       :status :not-found
       :chunks [(mood/flavor+freshness @(rf/subscribe [:core-db]) [opid])
                (:excerpt opinion)]]
      (= :warn-off cinfo)
      [thread-excerpt-display
       :status :warn-off
       :chunks [[:warn-off (:excerpt opinion)]]]
      (vector? cinfo)
      [thread-excerpt-display :chunks cinfo])))

(defn reference-root-display [reference & {:keys [minify hide-warstats hide-external-link]}]
  (let [warstats @(rf/subscribe [:warstats-store reference])]
    [:<>
     [tb/headline
      :domain (misc/url-domain reference)
      :rootid reference
      :url true]
     (when-not hide-external-link
       [tb/display-external-link :url reference :black true])
     (when-not hide-warstats
       [tb/display-warstats :warstats warstats :black true])]))

(defn reference-excerpt-display [refd & {:keys [minify hide-warstats hide-external-link]}]
  (let [opinion @(rf/subscribe [:opinion-store refd])
        warstats @(rf/subscribe [:warstats-store refd])
        popup-visible? @(rf/subscribe [:popup-is-active? refd])
        treead (when opinion (:tree-address opinion))
        deep (when treead (< 1 (count treead)))
        description (if (and opinion (excerpts/has-excerpt? opinion))
                      (if deep
                        "Excerpt from discussion of article at "
                        "Excerpt from article at ")
                      (if deep
                        "Opinion from discussion of article at "
                        "Opinion on article at "))]
    [:<>
     [rc/popover-anchor-wrapper
      :showing? popup-visible?
      :position :below-center
      :style {:display "inline" :flex-grow 0 :min-width "fit-content"}
      :parts {:point-wrapper {:style {:display "inline"}}}
      :anchor [:span
               {:class "italic font-thin truncate"
                :on-click (fn [e]
                            (rf/dispatch [:toggle-active-popup refd])
                            (.stopPropagation e))}
               (if opinion (str description (misc/url-domain (:rooturl opinion))) "")]
      :popover
      [rc/popover-content-wrapper
       :parts {:border
               {:class "sm:w-[70rem] text-black"
                :style {:background-color "rgba(255, 255, 255, 0.7)"
                        :box-shadow "rgba(0, 0, 0, 0.3) 0px 0px 8px"
                        :border-radius "3px"}}}
       :showing-injected? popup-visible?
       :on-cancel #(rf/dispatch [:toggle-active-popup refd])
       :backdrop-opacity 0.0
       :arrow-renderer deco/wf-arrow
       :arrow-length 44
       :body [opinion-info refd :show-excerpt true]]]
     (when-not hide-external-link
       [tb/display-external-link :url (:rooturl opinion) :black true])
     (when-not hide-warstats
       [tb/display-warstats :warstats warstats :black true])]))

;;FIXME: refactor -> *-tb-stuff
(defn reference [opinion & {:keys [minify style hide-warstats hide-external-link]}]
  (let [ref (or (:refd-opinion opinion) (:reference opinion))]
    [:div
     {:class "text-white bg-black flex flex-row items-center gap-4 pl-2 pb-0.5"
      :style style}
     [:img {:src "/static/img/white-reference.svg"
            :class (if minify "min-w-[21] h-[23] shrink-0" "min-w-[42px] h-[45px]")}]
     [(if (misc/iid? ref) reference-excerpt-display reference-root-display)
      ref :minify minify :hide-warstats hide-warstats :hide-external-link hide-external-link]]))

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

;;FIXME: refactor -> *-tb-stuff
(defn question-container [props & {:keys [body minify warstats]}]
  [:div
   (merge {:class "flex flex-row items-center bg-[#f5eb72] pl-1.5 gap-4"} props)
   [:img {:src (tb/question-icon warstats)
          :class (if minify "min-w-[21] h-[23]" "max-w-[42px] h-[45px]")}]
   body])

(defn question [opid & {:keys [minify style hide-warstats truncate]
                        :or {truncate :follow-minify}}]
  (when @(rf/subscribe [:opinion-store opid])
            [question-container
             {:style style}
             :minify minify
             :body
             [:<>
      ;;FIXME: Should manually truncate?
              [tb/headline :opinionid opid :url true :class (when (if (= :follow-minify truncate)
                                                                    minify
                                                                    truncate)
                                                              "truncate")]
              (when-not hide-warstats
                [tb/display-warstats :warstats @(rf/subscribe [:warstats-store opid])])]]))

(defn question-info [opid]
  (let [warstats @(rf/subscribe [:warstats-store opid])
        listof (misc/is-list-of-things? warstats)
        answered (misc/is-answered? warstats)]
    [question-container {}
     :warstats warstats
     :body
     (if listof
       [:<>
        [:span (tb/warstat-text :x-wrong-source (count (:x-wrong-source warstats)))]
        [:span (tb/warstat-text :x-right-source (count (:x-right-source warstats)))]]
       [:<>
        (if answered
          [:span {:class deco/casual} "Answered"]
          [:span {:class deco/casual} "Open"])])]))

(defn opinion-extras [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        warstats @(rf/subscribe [:warstats-store opid])]
    [:div
     {:class "sm:flex sm:flex-row child:sm:grow child:sm:basis-0"}
     (when (:reference opinion) [reference opinion])
     (when (:question warstats) [question-info opid])]))

;;FIXME: adjust nomenclature for various style and body inserts
(defn thread-opinion [& {:keys [opid text children no-tree-address substitutes style
                                body-style body hidden-text]}]
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
                   (or (:clean-comment parent) "")
                   text)
            small  @(rf/subscribe [:window-small?])
            tbar (if small
                   [:<>
                    [tb/author-long opinion]
                    children
                    [tb/display-warstats :warstats warstats]]
                   [:<>
                    [tb/flag-name opinion]
                    [tb/date-stamp opinion]
                    [tb/author-long opinion]
                    children
                    [tb/display-warstats :warstats warstats]
                    ;;FIXME: should handle excerpts, could use iid instead of url?
                    #_[tb/reply-link :target (:iid opinion) :excerpt @excerpt :offset @offset]])
            main-style
            (if
              small
              {:width "100%"}
              {:margin-left (deco/thread-opinion-indent (dec (count tree-address)))
               :width "80%"})]
        [(if small opinion-container-mobile opinion-container)
         {:class "mb-6 sm:break-normal break-all sm:break-words relative"
          :style (merge main-style style)
          ;;Was causing trouble for sub component clicks. Don't actually need.
          ;;To re-enable will need to constrain what clicks are accepted.
          ;;:on-click (fn [e]
          ;;            (set! (. js/window -location) (misc/make-opinion-url opinion))
          ;;            (.stopPropagation e))
          }
         :iconid opid
         :icon-layout (when small (if no-tree-address :icon :tree-address))
         :opinion opinion
         :substitutes substitutes
         :titlebar tbar
         :body
         [:<>
          [:div
           {:class "m-4 mt-1"
            :style body-style}
           body
           ;; {:overflow "overlay"} ??
           (when (excerpts/has-excerpt? opinion)
             [thread-excerpt :opinionid opid :text text])
           (when (:clean-comment opinion)
             [hilited-text
              :text (:clean-comment opinion)
              :tree-address tree-address
              :focus nil
              :hidden hidden-text
              :disable-popup? true
              ;;FIXME: Have we lost reply links?
              :excerpt nil
              :offset nil])]
          [opinion-extras opid]]]))))

(declare hidden-items unhidden-items)
(defn excerptless-opinions [target-id]
  (let [idlist @(rf/subscribe [:immediate-children target-id])
        show? @(rf/subscribe [:visibility-show-all])
        idlist (remove
                (fn [id]
                  (let [cinfo @(rf/subscribe [:excerpt-context-info id])]
                    (or (not cinfo) (= cinfo :not-found))))
                idlist)
        [vislist hidlist]
        (misc/splitfilter (fn [id]
                       (let [vis @(rf/subscribe [:visibility id])]
                         (= :show (:list-display vis))))
                     idlist)]
    [:div
     {:class "mt-4"}
     [:h3 "Replies:"]
     (into [:div] (map #(vector thread-opinion :opid %1) (if show? idlist vislist)))
     (if show?
       [unhidden-items (count hidlist)]
       [hidden-items hidlist])]))

(defn opinion-casual [opid]
  (let [opinion @(rf/subscribe [:opinion-store opid])
        text (if opinion
               (str "opinion by " (:authorname opinion))
               "unknown opinion")]
    [:span text]))


;; Visibility widgets and stuff

(defn thread-opinion-warn-off [& {:keys [opid] :as params}]
  (let [vis @(rf/subscribe [:visibility opid])
        warnoffs (:warn-off vis)
        _ (when (empty? warnoffs)
            (throw (js/Error. "No warn-offs! Why are we here?")))
        color (:color (get flags/flags (first (first warnoffs))))]
    (into [thread-opinion
           :body-style (assoc (vis/warn-off-style (first (first warnoffs)))
                              :margin "0px"
                              :padding "1rem")
           :hidden-text true
           :body
           [:h4 {:style {:position "absolute"}}
            (str "Not displayed because: "
                 (string/join " " (map (fn [[flag effect]]
                                         (get-in flags/flags [flag :label]))
                                       warnoffs)))]]
          cat params)))

(defn thread-opinion-selector [iid]
  (let [vis @(rf/subscribe [:visibility iid])]
    (when vis
      (if (or (empty? (:warn-off vis))
              (:warn-off-excerpt-only vis))
        [thread-opinion :opid iid]
        [thread-opinion-warn-off :opid iid]))))

(defn hidden-items [items]
  (when-not (empty? items)
    (let [vis @(rf/subscribe [:visibility])
          causes (map #(get-in vis [% :list-display]) items)
          mech (count (filter (partial = :mechanical) causes))
          tt (count (filter (partial = :text-title) causes))
          faded (count (filter (partial = :faded) causes))]
      ;;FIXME: add controls for override, perhaps only for logged-in
      (deco/casual-note-heading
       [:<>
        (str
         "Opinions not shown: "
         (when-not (zero? faded)
           (str faded " below threshold "))
         (when-not (zero? mech)
           (str mech " non content "))
         (when-not (zero? tt)
           (str tt " text/title thread")))
        [:a {:href (vis/set-show-all js/window.location.href true)} "Show All"]]))))

(defn unhidden-items [some]
  (when-not (zero? some)
    (deco/casual-note-heading
     [:<> "Showing All"
      [:a {:href (vis/set-show-all js/window.location.href false)}
       (str "Hide " some " item(s)")]])))

