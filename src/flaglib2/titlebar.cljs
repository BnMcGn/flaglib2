(ns flaglib2.titlebar
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [goog.uri.utils :as uri]
   [clojure.set :as set]

   [re-com-tailwind.core :as rc]
   [re-com-tailwind.functions :refer [tw-btn-default tw-btn]]

   [flaglib2.misc :as misc]
   [flaglib2.flags :as flags]
   [flaglib2.mood :as mood]
   [flaglib2.deco :as deco]))

(def indicator-names
  {:x-up "thumbs_up_sign"
   :x-down "thumbs_down_sign"
   :x-right "check_mark"
   :x-wrong "ballot_x"})

(def indicator-names-black
  {:x-up "thumbs_up_white"
   :x-down "thumbs_down_white"
   :x-right "check_mark_white"
   :x-wrong "ballot_x_white"})

(def warstat-text
  {:x-up "Has approval"
   :x-down "Has disapproval"
   :x-right "Has supporting evidence"
   :x-wrong "Has contradicting evidence"})

(defn flag-name [opinion]
  (let [flag (get flags/flags (:flag opinion))]
    [:span (str (:category flag) " " (:label flag))]))

(defn flag-icon [type]
  (let [flag (get flags/flags type)
        ;;FIXME: unknown flag should have unique icon
        color (if flag (subs (:color flag) 1) "fff")]
    (str "/static/img/small/wf_flag-" color ".svg")))

(defn question-icon [warstats]
  (let [question (:question warstats)
        answered (:question-answered warstats)
        listof (and question (set/intersection (set question) #{:tag :replies}))]
    (str "/static/img/black-wf-"
         (if listof "list-of-things" "question")
         (when answered "-a")
         ".svg")))

;; Might not need, dependent on need for tooltip
;;(defn opinion-icon-core [])

(defn opinion-icon [opid & {:keys [class style title]}]
  (let [opinion @(rf/subscribe [:opinion-store opid])]
    [:a
     {:href (misc/make-opinion-url opinion)
      :class class
      :title title
      :style (or style {})}
     [:img {:src (flag-icon (:flag opinion)) :class "inline"}]]))

(defn opinion-icon-tt [opid & {:keys [class style supply? description]}]
  [opinion-icon
   opid
   :class (misc/class-string class (if supply? "border-green-300" "border-black") "border-2")
   :title (if supply?
            (str "Supplies the active " description)
            (str "Suggests a " description))
   :style style])

(defn display-tree-address [tree-address & {:keys [style class]}]
  [rc/h-box
   :style style
   :class class
   :children
   (rest
    (reduce into []
            (for [id tree-address]
              [" > " [opinion-icon id]])))])

;;FIXME: might want magnitude to adjust proportionately to other axes
(defn display-warstats [& {:keys [warstats target-id class black]}]
  (let [warstats (or warstats
                     @(rf/subscribe [:warstats-store target-id]))]
    [rc/h-box
     :class (str "mr-3 " class)
     :children
     (into []
           (map
            (fn [axis]
              (let [stat (get warstats axis)
                    mag (if (integer? stat) (mood/magnitude stat) 0)
                    opacity (if black "opacity-50" "opacity-25")
                    opacity (when (or (not stat) (zero? stat)) (str " " opacity))
                    mags (if (#{:x-up :x-right} axis)
                           deco/positive-magnitude
                           deco/negative-magnitude)]
                [:span
                 {:class (str (nth mags mag) opacity)}
                 [:img {:src (str "/static/img/" (get (if black
                                                        indicator-names-black
                                                        indicator-names) axis) ".svg")
                        :style {:width "12px" :height "12px"}
                        :title (when (and stat (not (zero? stat)))
                                 (get warstat-text axis))}]]))
            '(:x-up :x-down :x-right :x-wrong)))]))

(defn date-stamp [opinion]
  (let [created (:created opinion)
        [quantity unit] (misc/ago (:created opinion))]
    (when created
      [:span (str quantity " " unit " ago")])))

(defn author-long [opinion]
  (let [auth (or (:authorname opinion) (:author opinion))]
    [:a {:style {:color "black"}
         :href (misc/make-author-url auth)} auth]))

(defn reply-link-menu [body & {:keys [popover-content button-label]}]
  (let [menu? (r/atom nil)
        tooltip? (r/atom nil)]
    [rc/popover-anchor-wrapper
     :class "mr-3"
     :showing? (and (seq popover-content) tooltip? (not menu?))
     :position :below-right
     :popover [rc/popover-content-wrapper
               :body popover-content]
     :anchor
     [rc/popover-anchor-wrapper
      :showing? menu?
      :position :below-right
      :anchor   [rc/button
                 :class (tw-btn (tw-btn-default))
                 :label button-label
                 :attr {:on-mouse-over #(do (reset! tooltip? true) nil)
                        :on-mouse-out  #(do (reset! tooltip? false) nil)}
                 :on-click #(do (swap! menu? not) false)]
      :popover  [rc/popover-content-wrapper
                 :arrow-width 0
                 :arrow-length 0
                 :close-button? false
                 :body body]]]))

(defn target-link-url [& {:keys [target excerpt offset flag title-or-text suggest]}]
  (let [base "/opinion/"
        alt-key (when title-or-text
                  (if suggest
                    (case title-or-text
                      :title :suggest-target-title
                      :text :suggest-target-text)
                    (case title-or-text
                      :title :target-title
                      :text :target-text)))
        params (cond-> {:target target}
                 alt-key (assoc alt-key "true")
                 excerpt (assoc :excerpt excerpt)
                 offset (assoc :offset offset)
                 flag (assoc :flag flag))]
    (uri/appendParamsFromMap base (clj->js params))))

(defn reply-link [& {:keys [target excerpt offset]}]
  [reply-link-menu
   [:div
    [:div [:a
           {:class "text-black"
            :style {:color "black"}
            :href (target-link-url
                   :target target :excerpt excerpt :offset offset
                   :flag :positive-like)}
           "Upvote"]]
    [:div [:a
           {:class "text-black"
            :style {:color "black"}
            :href (target-link-url
                   :target target :excerpt excerpt :offset offset
                   :flag :negative-dislike)}
           "Downvote"]]
    [:div [:a
           {:class "text-black"
            :style {:color "black"}
            :href (target-link-url
                   :target target :excerpt excerpt :offset offset
                   :flag :custodial-blank)}
           "Comment"]]
    [:div [:a
           {:class "text-black"
            :style {:color "black"}
            :href (target-link-url
                   :target target :excerpt excerpt :offset offset)}
           "Other..."]]]
   :popover-content (when excerpt (str "Reply to the excerpt: \"" excerpt "\""))
   :button-label (if (seq excerpt) "Reply to Excerpt" "Reply")])

(defn reply-link-tt [& {:keys [target excerpt offset hide-text]}]
  [reply-link-menu
   [:div
    [:div [:a
           {:style {:color "black"}
            :href (target-link-url
                   :flag :custodial-blank
                   :target target :title-or-text :title)}
           "Comment on title"]]
    (when-not hide-text
      [:div [:a
             {:style {:color "black"}
              :href (target-link-url
                     :flag :custodial-blank
                     :target target :title-or-text :text)}
             "Comment on text"]])
    [:div [:a
           {:style {:color "black"}
            :href (target-link-url
                   :flag :custodial-blank
                   :target target :title-or-text :title :suggest true)}
           "Supply article title"]]
    (when-not hide-text
      [:div [:a
             {:style {:color "black"}
              :href (target-link-url
                     :flag :custodial-blank
                     :target target :title-or-text :text :suggest true)}
             "Supply article text"]])]
   :button-label (if hide-text "Discuss Title" "Discuss Title/Text")])

;;Old, simple version that might still be useful...
(defn reply-link-x [& {:keys [target excerpt offset]}]
  [:form
   {:class "inline-block relative text-sm mr-3"
    ;;FIXME: Why doesn't form vertically center like other elements?
    :style {:top "0.5em"}
    :action "/opinion/" :method "GET"}
   [:input {:type "hidden" :name "target" :value (or target "")}]
   (when-not (empty? excerpt)
     [:input {:type "hidden" :name "excerpt" :value excerpt}])
   (when offset
     [:input {:type "hidden" :name "offset" :value (or offset "")}])
   (if (and excerpt (not (empty? excerpt)))
     [:input
      {:type "submit"
       :title (str "Reply to the excerpt: \"" excerpt "\"")
       :value "Reply to Excerpt"
       :class (tw-btn (tw-btn-default))}]
     [:input {:type "submit" :value "Reply" :class (tw-btn (tw-btn-default))}])])

(defn reply-count [& {:keys [warstats class]}]
  (let [immediate (:replies-immediate warstats)
        total (:replies-total warstats)]
    [:span {:class ["text-base mr-3" class]
            :title (str immediate " direct responses, " total " in conversation")}
     (when (and immediate total) (str " (" immediate "/" total ")"))]))

(defn display-external-link [& {:keys [url black]}]
  [:a {:href url :class "mr-3"}
   [:img {:src (if black "/static/img/white-external-link.svg" "/static/img/black-external-link.svg")
          :style {:min-width "22px"}
          :alt "Original article" :title "Original article"}]])

(defn headline [& {:keys [title url domain rootid opinionid class style truncate no-fontsize]}]
  (when (and rootid opinionid)
    (throw (js/Error. "Can only use one of rootid or opinionid")))
  (let [id (or rootid opinionid)
        [titl available? patch?]
        (if title
          [title true false]
          @(rf/subscribe [:title-summary id]))
        fontsize (if no-fontsize "" (if available? "text-lg" "font-thin"))
        truncate (or truncate (if available? false true))
        truncate (when truncate "truncate")
        italic (when-not available? "italic")
        patch (when (and (not domain) patch?) deco/patch)
        domain (when domain (str "(" domain ")"))
        class [class (misc/class-string "mx-3" fontsize italic truncate patch)]
        core (if domain
               (if patch?
                 [[:span {:class deco/patch} titl] (str " " domain)]
                 [(str titl " " domain)])
               [titl])
        url (cond
              (string? url) url
              (true? url) (cond rootid (misc/make-target-url rootid)
                                opinionid (misc/make-opinion-url {:iid opinionid})
                                :else false)
              :else false)]
    (if url
      [:span {:class class :style style} (into [:a {:href url}] core)]
      (into [:span {:class class :style style}] core))))


;;; Unified titlebar info system


(defn root-tb-stuff [url db & {:keys [reply-excerpt reply-offset warstats]}]
  (let [warstats (or warstats (get (:warstats-store db) url))]
    {:external-link [display-external-link :url url]
     :warstats [display-warstats :warstats warstats]
     :reply-link [reply-link :target url :excerpt reply-excerpt :offset reply-offset]
     :count [reply-count :warstats warstats]
     :bg-color ((mood/flavor-from-own-warstats warstats) deco/flavor-background)
     :headline [headline :rootid url :url true]}))

(defn opinion-tb-stuff [iid db & {:keys [reply-excerpt reply-offset warstats]}]
  (let [warstats (or warstats (get (:warstats-store db) iid))
        opinion (get-in db [:warstats-store iid])]
    {:tree-address [display-tree-address opinion]
     :icon (flag-icon (:flag opinion))
     :flag-name [flag-name opinion]
     :opinion-icon [opinion-icon iid]
     :date-stamp [date-stamp opinion]
     :author-long [author-long opinion]
     :warstats [display-warstats :warstats warstats]
     :count [reply-count :warstats warstats]
     :comment? (not (empty? (:clean-comment opinion)))
     :bg-color ""
     :reply-link [reply-link :target iid :excerpt reply-excerpt :offset reply-offset]
     :headline [headline :opinionid iid :url true]}))

(defn question-tb-stuff [iid db & {:keys [warstats]}]
  (let [warstats (or warstats (get-in db [:warstats-store iid]))]
     {:icon (question-icon warstats)
      :icon-size "max-w-[42px] h-[45px]"
      :icon-size-mini "min-w-[21px] h-[23px]"
      :bg-color "bg-[#f5eb72]"
      :warstats [display-warstats :warstats warstats]
      :headline [headline :opinionid iid :url true]}))

(defn reference-tb-stuff [reference db & {:keys [warstats]}]
  (let [warstats (or warstats (get-in db [:warstats-store reference]))]
    {:icon "/static/img/white-reference.svg"
     :icon-size "min-w-[42px] h-[45px]"
     :icon-size-mini "min-w-[21px] h-[23px]"
     :bg-color "bg-black"
     :warstats [display-warstats :warstats warstats :black true]
     :external-link [display-external-link :url reference :black true]
     :headline [headline :rootid reference :url true :class "text-white"]}))

(defn author-tb-stuff [author db]
  ;;Use fake opinion
  {:author-long [author-long {:author author}]})

(defn assemble-bar-parts [stuff reqlist]
  (filter identity (map #(if (keyword? %1) (%1 stuff) %1) reqlist)))

(defn rewidget-item [orig sub]
  (if sub
    (let [sub (if (symbol? sub) [sub] sub)]
     (reduce into [[(first sub)] (rest orig) (rest sub)]))
    orig))

(defn rewidget-stuff [stuff substitutes]
  (merge stuff
         (into {}
               (for [[k v] substitutes]
                 [k (rewidget-item (get stuff k) v)]))))

