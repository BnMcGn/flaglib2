(ns flaglib2.target
  (:require
   [re-frame.alpha :as rf]
   [reagent.core :as r]
   [goog.uri.utils :as uri]

   [cljsjs.rangy-textrange]
   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs :as ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.displayables :as disp]
   [flaglib2.visibility :as vis]

   [flaglib2.target-summary :as tsum]
   [flaglib2.titlebar :as tb]))


(defn target-root-article-core [& {:keys [rooturl]}]
  (let [excerpt (r/atom "")
        offset (r/atom nil)]
    (fn [& _]
     [:div
      [disp/root-title
       :display-depth 0
       :url rooturl
       :intro-text "Article: "
       :reply-excerpt @excerpt
       :reply-offset @offset]
      [deco/casual-heading (str "Text from article at " (misc/url-domain rooturl))]
      [disp/hilited-text
       :text-key rooturl
       :root-target-url rooturl
       :tree-address (list)
       :excerpt excerpt
       :offset offset]
      [disp/excerptless-opinions rooturl]])))

(defn text-missing-report []
  (let [{:keys [rooturl touched-p refd]} @(rf/subscribe [:server-parameters])
        uname (misc/username)
        {:keys [initial-message initial-status]}
        (if touched-p
          @(rf/subscribe [:text-store rooturl])
          {})
        advanced @(rf/subscribe [:advanced-options])
        reason (if touched-p
                 initial-message
                 "No discussion has been started on the article, so text extraction probably has not been attempted.")]
    [:div
     [disp/root-title :url rooturl :intro-text "Article: " :display-depth 0]
     [deco/casual-heading
      (if advanced
        (str "Text from article at " (misc/url-domain rooturl) " is not currently available")
        (str "Discussion of article at " (misc/url-domain rooturl)
             " has not been opened yet"))]
     (when advanced
       [:h4 (str "Reason: " reason)])
     [:ul
      (when (not-empty refd)
        (let [ct (count refd)]
          [:li (if (= 1 ct)
                 "1 reference has been made to this URL"
                 (str ct " references have been made to this URL"))
           ]))
      (when advanced
        [:<>
         (when-not touched-p
           [:li "Text extraction will be attempted by the system if you start a new post"])
         [:li "You may still post flags on this article, though excerpts must be filled by hand"]
         [:li "Alternate texts and titles may be manually inserted under the Text/Title tab"]
         ])]]))

(defn target-root-article [& {:keys [rooturl]}]
  (let [text-info @(rf/subscribe [:text-store rooturl])]
    (if (and text-info (not-empty (:text text-info)))
      [target-root-article-core :rooturl rooturl]
      [text-missing-report])))

(defn target-root-thread [& {:keys [rooturl]}]
  (let [optree @(rf/subscribe [:visible-tree rooturl])
        offtree @(rf/subscribe [:invisible-tree rooturl])]
    [:div
     [disp/root-title :url rooturl :intro-text "Article: " :display-depth 0]
     (when optree
       (into [:<>]
             (map (fn [opid]
                    [disp/thread-opinion-selector opid])
                  (keep identity (flatten optree)))))
     [disp/hidden-items (keep identity (flatten offtree))]]))

;;FIXME: Add title excerpt support
;;FIXME: Display original title, maybe text?
(defn text-title-thread [& {:keys [rooturl]}]
  (let [title-tree @(rf/subscribe [:title-tree rooturl])
        text-tree @(rf/subscribe [:text-tree rooturl])
        db @(rf/subscribe [:core-db])]
    [:div
     [disp/root-title :url rooturl :display-depth 0 :tt true]
     (when-not (empty? title-tree)
       (into [:<> [:h3 "Title discussion:"]]
             (map (fn [opid]
                    (let [opinion (get-in db [:opinion-store opid])
                          indicate (and (not (misc/deep-opinion? opinion))
                                        (misc/opinion-suggests-tt? opinion))
                          subs (when indicate
                                 {:opinion-icon
                                  [tb/opinion-icon-tt
                                   :description "title"
                                   :supply? (misc/opinion-supplies-title? opinion db)]})]
                      [disp/thread-opinion :opid opid :substitutes subs]))
                  (flatten title-tree))))
     (when-not (empty? text-tree)
       (into [:<> [:h3 "Text discussion:"]]
             (map (fn [opid]
                    (let [opinion (get-in db [:opinion-store opid])
                          indicate (and (not (misc/deep-opinion? opinion))
                                        (misc/opinion-suggests-tt? opinion))
                          subs (when indicate
                                 {:opinion-icon
                                  [tb/opinion-icon-tt
                                   :description "text"
                                   :supply? (misc/opinion-supplies-text? opinion db)]})]
                      [disp/thread-opinion :opid opid :substitutes subs]))
                  (flatten text-tree))))]))

(defn target-root-core []
  (let [params @(rf/subscribe [:server-parameters])
        current (r/atom (if-let [tmode (:tmode params)]
                          (keyword tmode)
                          :article))
        advanced @(rf/subscribe [:advanced-options])
        small @(rf/subscribe [:window-small?])]
    [:<> [(if small rc/vertical-bar-tabs rc/horizontal-tabs)
          :model current
          :tabs [{:id :article :label "Article View"}
                 {:id :comment :label "Comment View"}
                 {:id :summary :label "Summary"}
                 (when advanced {:id :tt :label "Title/Text"})]
          :parts {:wrapper {:class "mt-2"}
                  :anchor {:style {:color "#777"}}}
          :on-change #(set! js/window.location.href
                            (uri/setParam js/window.location.href "tmode" (name %1)))]
     (case @current
       :article [target-root-article :rooturl (:rooturl params)]
       :comment [target-root-thread :rooturl (:rooturl params)]
       :summary [tsum/target-stats :rooturl (:rooturl params)]
       :tt [text-title-thread :rooturl (:rooturl params)])]))

(defn target-page-title [rooturl db]
  (let [tinfo (ipfs/get-any-title db rooturl)
        title (misc/has-title? tinfo)
        domain (misc/url-domain rooturl)]
    (if title
      (str "WF: (" domain ") " title)
      (str "WF: Article at " domain))))

(rf/reg-event-fx
 ::set-target-page-headers
 (fn [{:keys [db]} [_ context]]
   (let [newdb (get-in context [:effects :db])
         event (get-in context [:coeffects :event])
         eurl (second event)
         rooturl (get-in db [:server-parameters :default :rooturl])]
     (when (= rooturl eurl)
       {:set-page-title (target-page-title rooturl newdb)
        :set-social-meta {:title "WarFlagger.net" :id eurl
                          :description (str "Discussion of article at "
                                            (misc/url-domain eurl))}}))))

(defn target-root []
  (let [{:keys [rooturl touched-p]} @(rf/subscribe [:server-parameters])
        uname (misc/username)]
    ;;Has a text been extracted/provided? Yes? good!
    ;;No? Is user logged in? Yes? user can do something about it.
    ;;No? Nothing to do.
    (if (or uname touched-p)
      [target-root-core]
      [text-missing-report])))

(rf/reg-event-fx
 :target
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :default :rooturl])
         db (assoc db :root-element target-root)]
     {:db db
      :fx [ [:dispatch [:flaglib2.ipfs/request-rooturl-item target "opinion-tree"]]
            [:dispatch [:add-after-hooks
                        {:flaglib2.ipfs/received-title
                         [::set-target-page-headers :flaglib2.misc/context]}]]
            [:dispatch [:load-rooturl target]]
            [:dispatch [:mount-registered]]]})))
