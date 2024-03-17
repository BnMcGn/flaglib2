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


(defn target-root-article [& {:keys [rooturl]}]
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

(defn target-root []
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

(rf/reg-event-fx
 :target
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :default :rooturl])
         db (assoc db :root-element target-root)]
     {:db db
      :fx [ [:dispatch [:flaglib2.ipfs/request-rooturl-item target "opinion-tree"]]
            [:dispatch [:load-rooturl target]]
           [:dispatch [:mount-registered]]]})))
