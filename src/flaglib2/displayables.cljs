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
   [flaglib2.hilited :as hi]
   [flaglib2.titlebar :as tb]
   [re-com-tailwind.core :as rc]))

(def rangy js/rangy)

;;FIXME: refactor -> *-tb-stuff
(defn root-title-display [props & {:keys [url title intro-text hide-warstats
                                          warstats hide-reply hide-count reply-excerpt reply-offset
                                          hide-external-link warflagger-link children reorder
                                          headline-style headline-class]}]

  (let [intro (when intro-text [:span {:class "font-bold"} intro-text])
        head [tb/headline :title title :rootid url :url true :style headline-style :class headline-class]
        link (when (and url (not hide-external-link))
               [tb/display-external-link :url url])
        ws (when-not hide-warstats
             [tb/display-warstats :warstats warstats])
        rep (when-not hide-reply
              [tb/reply-link :url url :excerpt reply-excerpt :offset reply-offset])
        ct (when-not hide-count
             [tb/reply-count :warstats warstats])]
    (into [:div props]
          (if reorder
            [intro head link rep ws children count]
            [intro head link ws children rep count]))))

(defn root-title [& {:keys [style url display-depth warstats no-grid class] :as args}]
  (let [small @(rf/subscribe [:window-small?])
        dd (nth deco/display-depths (or display-depth 0))
        warstats (or warstats @(rf/subscribe [:warstats-store url]))
        flavor ((mood/flavor-from-own-warstats warstats) deco/flavor-background)
        box (if no-grid
              ""
              (if small
                "grid-cols-2 grid child:justify-self-center child:self-center gap-y-0.5 pt-2"
                "flex flex-row items-center"))
        class (misc/class-string dd flavor box class)
        props {:class class :style style}]
    (reduce into [root-title-display props :reorder small] (assoc args :warstats warstats))))

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

(defn opinion-container-mobile [props & {:keys [opinion titlebar body box-props icon-style]
                                         :or {icon-style :tree-address}}]
  [:div
   (or props {})
   (case icon-style
     :tree-address [tb/display-tree-address (:tree-address opinion)]
     :icon [tb/opinion-icon (:iid opinion)]
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

(defn opinion-info [opid]
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
      (when-not (empty? (:comment opinion))
        ;;FIXME: should be clean comment?
        [:div (excerpts/rebreak (:comment opinion))])
      (when (:reference opinion)
        [reference (:reference opinion)])]]))

;;; Opinion-summary is used to display opinions in one line situations. It may be displayed with
;;; tree address icons.
;;FIXME: Would be nice to have an icon that indicates presence of a comment
(defn opinion-summary [opid & {:keys [hide-tree-address hide-icon hide-reply]}]
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
        opid (or opinionid (:id opinion))
        {:keys [leading trailing excerpt]}
        (cond (and opinion (:leading opinion)) opinion
              text (let [tpos (:text-position opinion)]
                     (excerpts/excerpt-context text (nth tpos 0) (nth tpos 1)))
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

(defn reference-excerpt-display [])

;;FIXME: refactor -> *-tb-stuff
(defn reference [reference & {:keys [minify style hide-warstats hide-external-link]}]
  [:div
   {:class "text-white bg-black flex flex-row items-center gap-4 pl-2 pb-0.5"
    :style style}
   [:img {:src "/static/img/white-reference.svg"
          :class (if minify "min-w-[21] h-[23]" "min-w-[42px] h-[45px]")}]
   [(if (misc/iid? reference) reference-excerpt-display reference-root-display)
    reference :minify minify :hide-warstats hide-warstats :hide-external-link hide-external-link]])

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
(defn question-container [props & {:keys [body minify]}]
  [:div
   (merge {:class "flex flex-row items-center bg-[#f5eb72] pl-1.5 gap-4"} props)
   [:img {:src "/static/img/black-wf-question.svg"
          :class (if minify "min-w-[21] h-[23]" "max-w-[42px] h-[45px]")}]
   body])

(defn question [opid & {:keys [minify style hide-warstats truncate]
                        :or {truncate :follow-minify}}]
  (when-let [opinion @(rf/subscribe [:opinion-store opid])]
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
                small  @(rf/subscribe [:window-small?])
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
