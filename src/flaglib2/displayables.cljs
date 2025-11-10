(ns flaglib2.displayables
  (:require
   [re-frame.alpha :as rf]
   [reagent.core :as r]
   [clojure.set :as set]

   [cljsjs.rangy-textrange]

   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]
   [flaglib2.excerpts :as excerpts]
   [flaglib2.hilited :as hi]
   [flaglib2.titlebar :as tb]
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
                        excerpt offset grey?]}]
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
   :sub-opin-component sub-opinion-list])

(defn quote-icon [& {:keys [found style]}]
  [:img {:style style
         :src (if found
                "/static/img/black-wf-quote.svg"
                "/static/img/red-wf-quote.svg")
         :title (when-not found "Excerpt Not Found")}])

(defn thread-excerpt-display
  [& {:keys [leading-context trailing-context excerpt excerpt-class]}]
  (let [icon-style {:width "42px" :height "45px" :float "left" :top "-1em" :margin-right "1em"}]
    [:div
     {:class "thread-excerpt italic text-sm mt-2 mb-4 sm:mr-40 mr-6 min-h-[3em] ml-6 sm:break-normal break-all sm:break-words"}
     [quote-icon :found (or leading-context trailing-context) :style icon-style]
     [:span {:style {:background-color "#eee"}}
      [:span (excerpts/rebreak leading-context)]
      [:span {:class excerpt-class} (excerpts/rebreak excerpt)]
      [:span (excerpts/rebreak trailing-context)]]]))

(defn thread-excerpt
  [& {:keys [opinion opinionid text]}]
  (let [opinion (or opinion
                    @(rf/subscribe [:opinion-store opinionid]))
        opid (or opinionid (:iid opinion))
        tpos @(rf/subscribe [:text-position-recalc opid])
        {:keys [text-position excerpt offset]} opinion
        {:keys [leading trailing excerpt]}
        (cond (= tpos :original) opinion
              (and tpos text) (excerpts/excerpt-context text (first tpos) (second tpos))
              tpos (let [text @(rf/subscribe [:proper-text (:target opinion)])]
                     (excerpts/excerpt-context text (first tpos) (second tpos)))
              :else opinion)]
    [thread-excerpt-display
     :leading-context leading :trailing-context trailing :excerpt excerpt
     :excerpt-class (mood/flavor+freshness @(rf/subscribe [:core-db]) [opid])]))


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
            :class (if minify "min-w-[21] h-[23]" "min-w-[42px] h-[45px]")}]
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

(defn thread-opinion [& {:keys [opid text children no-tree-address substitutes style]}]
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
           {:class "m-4 mt-1"}
           ;; {:overflow "overlay"} ??
           (when (excerpts/has-excerpt? opinion)
             [thread-excerpt :opinionid opid :text text])
           (when (:clean-comment opinion)
             [hilited-text
              :text (:clean-comment opinion)
              :tree-address tree-address
              :focus nil
              :disable-popup? true
              ;;FIXME: Have we lost reply links?
              :excerpt nil
              :offset nil])]
          [opinion-extras opid]]]))))

(defn excerptless-opinions [target-id]
  (let [opstore @(rf/subscribe [:opinion-store])
        idlist @(rf/subscribe [:immediate-children target-id])
        cdb @(rf/subscribe [:core-db])
        idlist (remove (partial excerpts/recalc-text-position cdb) idlist)]
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

