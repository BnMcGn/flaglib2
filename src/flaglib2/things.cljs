(ns flaglib2.things
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.displayables :as disp]
   [flaglib2.titlebar :as tb]))

(defn display-thing [tbstuff & {:keys [fields]}]
  (into [:div {:class (:bg-color tbstuff)}] (tb/assemble-bar-parts tbstuff fields)))

(defn display-thing-short [tbstuff & {:keys [fields]}]
  (let [tbstuff (update tbstuff :headline into [:no-fontsize true])]
    (into [:div {:class (misc/class-string (:bg-color tbstuff) "grid")}]
          (tb/assemble-bar-parts tbstuff fields))))

(defn display-thing-opinion [tbstuff & {:keys [fields]}]
  (into [:div {:class (:bg-color tbstuff)}] (tb/assemble-bar-parts tbstuff fields)))

(defn display-thing-opinion-short [tbstuff & {:keys [fields]}]
  (let [tbstuff (update tbstuff :headline into [:no-fontsize true])]
    [:div (into [:span {:class (misc/class-string (:bg-color tbstuff))}]
           (tb/assemble-bar-parts tbstuff fields))]))

(defn thing-displayer [things & {:keys [trim]}]
  (let [db @(rf/subscribe [:core-db])
        short (< trim 20)
        thing-element (if short display-thing-short display-thing)]
    (into [:<>]
          (for [{:keys [id type hide-author as-reference]} things]
            (case type
              :rooturl
              (if as-reference
                [thing-element
                 (tb/reference-tb-stuff id db)
                 :fields (if short
                           [:headline]
                           [:headline :warstats])]
                [thing-element
                 (tb/root-tb-stuff id db)
                 :fields (if short
                           [:headline]
                           [:headline :warstats :reply-count])])
              :opinion
              (let [tbstuff (tb/opinion-tb-stuff id db)]
                [(if short display-thing-opinion-short display-thing-opinion)
                 tbstuff
                 :fields (into [:opinion-icon] (if short
                                         (if hide-author [:headline] [:author-long])
                                         [:flag-name :date-stamp :author-long :headline]))])
              :author
              [thing-element
               (tb/author-tb-stuff id db)
               :fields [:author-long]])))))

(defn thing-lister [key]
  (let [spec @(rf/subscribe [:server-parameters key])]
    [thing-displayer (:things2 spec) :trim (:trim spec)]))

(defn process-things [things]
  (map #(update %1 :type keyword) things))

(defn thing-loaders [things]
  (into []
        (for [{:keys [type id]} things
              :when (#{:rooturl :opinion} type)]
          (case type
            :rooturl
            [:dispatch [:load-rooturl id :no-text true :no-references true]]
            :opinion
            [:dispatch [:load-opinion id]]
            :author
            nil ;May need later
            ))))

(rf/reg-event-fx
 :thing-lister
 (fn [{:keys [db]} [_ key]]
   (let [spec (get-in db [:server-parameters key])
         things (process-things (:things spec))
         loaders (thing-loaders things)]
     {:db (update-in db [:server-parameters key] assoc
                     :side-element thing-lister
                     :things2 things)
      :fx (into loaders
                [[:dispatch [:mount-registered key]]])})))
