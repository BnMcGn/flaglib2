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
   [flaglib2.displayables :as disp]))


(defn target-root-article [& {:keys [focus rooturl]}]
  (let [size @(rf/subscribe [:window-size])
        rt (if (= size :xs) disp/root-title-mobile disp/root-title)]
    [:div
     [rt
      :display-depth 0
      :url rooturl
      :intro-text "Article: "]
     [deco/casual-heading (str "Text from article at " (misc/url-domain rooturl))]
     [disp/hilited-text
      :text-key rooturl
      :root-target-url rooturl
      :tree-address (list)
      ;;[disp/excerptless-opinions]
      ]]))

(defn target-root []
  (let [params @(rf/subscribe [:server-parameters])
        current (r/atom (if-let [tmode (:tmode params)]
                          (keyword tmode)
                          :article))]
    [:<> [rc/horizontal-tabs
          :model current
          :tabs [{:id :article :label "Article View"}
                 {:id :comment :label "Comment View"}
                 {:id :summary :label "Summary"}]
          :on-change #(set! js/window.location.href
                            (uri/setParam js/window.location.href "tmode" (name %1)))]
     [target-root-article :rooturl (:rooturl params)]]))

(rf/reg-event-fx
 :target
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :rooturl])
         tmode (get-in db [:server-parameters :tmode])
         db (assoc db :root-element target-root)]
     {:db db
      :fx [ [:dispatch [:flaglib2.ipfs/request-rooturl-item target "opinion-tree"]]
            [:dispatch [:load-rooturl target]]
            [:mount-registered db]]})))
