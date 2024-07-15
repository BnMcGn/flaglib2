(ns flaglib2.target
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [goog.uri.utils :as uri]

   [cljsjs.rangy-textrange]
   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.displayables :as disp]

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

(defn text-missing []
  (let [{:keys [rooturl touched-p refd]} @(rf/subscribe [:server-parameters])
        {:keys [initial-message initial-status]}
        (if touched-p
          @(rf/subscribe [:text-store rooturl])
          {})
        reason (if touched-p
                 initial-message
                 "No discussion has been started on the article, so text extraction probably has not been attempted.")]
    [:div
     [disp/root-title :url rooturl :intro-text "Article: " :display-depth 0]
     [deco/casual-heading
      (str "Text from article at " (misc/url-domain rooturl) " is not currently available")]
     [:h4 (str "Reason: " reason)]
     [:ul
      (unless touched-p
              [:li "Text extraction will be attempted by the system if you start a new post"])
      [:li "You may still post flags on this article, though excerpts must be filled by hand"]
      #_[:li "If the same text is available at another URL, please indicate the alternative with the SameThing flag."]
      [:li "Alternate texts and titles may be manually inserted under the Text/Title tab"]]]))

(defn target-root-article [& {:keys [rooturl]}]
  (let [text-info @(rf/subscribe [:text-store])]
    (if (and text-info (not-empty (:text text-info)))
      [target-root-article-core :rooturl rooturl]
      [text-missing])))

(defn target-root-thread [& {:keys [rooturl]}]
  (let [optree @(rf/subscribe [:normal-tree rooturl])]
    [:div
     [disp/root-title :url rooturl :intro-text "Article: " :display-depth 0]
     (when optree
       (into [:<>]
             (map (fn [opid]
                    [disp/thread-opinion :opid opid])
                  (flatten optree))))]))

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
        small @(rf/subscribe [:window-small?])]
    [:<> [(if small rc/vertical-bar-tabs rc/horizontal-tabs)
          :model current
          :tabs [{:id :article :label "Article View"}
                 {:id :comment :label "Comment View"}
                 {:id :summary :label "Summary"}
                 {:id :tt :label "Title/Text"}]
          :parts {:wrapper {:class "mt-2"}
                  :anchor {:style {:color "#777"}}}
          :on-change #(set! js/window.location.href
                            (uri/setParam js/window.location.href "tmode" (name %1)))]
     (case @current
       :article [target-root-article :rooturl (:rooturl params)]
       :comment [target-root-thread :rooturl (:rooturl params)]
       :summary [tsum/target-stats :rooturl (:rooturl params)]
       :tt [text-title-thread :rooturl (:rooturl params)])]))

(defn text-missing-userless []
  (let [{:keys [rooturl touched-p refd]} @(rf/subscribe [:server-parameters])
        ;;FIXME: Should be from subscription?
        login-url (misc/make-login-url)]
    [:div
     [disp/root-title :url rooturl :intro-text "Article: " :display-depth 0]
     [deco/casual-heading
      (str "Text from article at " (misc/url-domain rooturl) " is not currently available")]
     [:h4 "Reason: No discussion has been started on the article, so text extraction probably has not been attempted."]
     [:ul
      (when (not-empty refd)
        (let [ct (count refd)]
          [:li (if (= 1 ct)
                 "1 reference has been made to this URL"
                 (str ct " references have been made to this URL"))]))
      [:li [:a {:href login-url} "Log In"] " to start a discussion of this article"]]]))

(defn target-root []
  (let [{:keys [rooturl touched-p]} @(rf/subscribe [:server-parameters])
        uname (misc/username)]
    ;;Has a text been extracted/provided? Yes? good!
    ;;No? Is user logged in? Yes? user can do something about it.
    ;;No? Nothing to do.
    (if (or uname touched-p)
      [target-root-core]
      [text-missing-userless])))

(rf/reg-event-fx
 :target
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :default :rooturl])
         db (assoc db :root-element target-root)]
     {:db db
      :fx [ [:dispatch [:flaglib2.ipfs/request-rooturl-item target "opinion-tree"]]
            [:dispatch [:load-rooturl target]]
           [:dispatch [:mount-registered]]]})))
