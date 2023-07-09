(ns flaglib2.target
  (:require
   [re-frame.core :as rf]

   [cljsjs.rangy-textrange]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.displayables :as disp]))


(defn target-root-article [{:keys [focus rooturl]}]
  (let []
    [:div
     [disp/root-title
      :url rooturl
      :intro-text "Article: "]
     [deco/casual-note-heading "Text from article at " (misc/url-domain rooturl)]
     [disp/hilited-text
      :text-key rooturl
      :root-target-url rooturl
      :tree-address (list)
      ;;[disp/excerptless-opinions]
      ]]))


(defn target-root []
  (let [params @(rf/subscribe [:server-parameters])]
    [target-root-article :rooturl (:target :params)]))

(rf/reg-event-fx
 :target
 (fn [{:keys [db]} _]
   (let [target (get-in db [:server-parameters :target])
         tmode (get-in db [:server-parameters :tmode])
         db (assoc db :root-element target-root)]
     {:db db
      :fx [ [:dispatch [:flaglib2.ipfs/request-rooturl-item target "opinion-tree"]]
            [:dispatch [:load-rooturl target]]
            [:mount-registered db]]})))
