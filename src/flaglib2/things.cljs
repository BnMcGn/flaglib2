(ns flaglib2.things
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.displayables :as disp]
   [flaglib2.titlebar :as tb]))

(defn display-thing [tbstuff {:keys [fields truncate]}]
  (let [tbstuff (if truncate
                  (update tbstuff :headline into [:truncate true])
                  tbstuff)]
    (into [:div] (tb/assemble-bar-parts tbstuff fields))))

(defn thing-displayer [things & {:keys [trim]}]
  (let [db @(rf/subscribe [:core-db])
        short (< trim 20)]
    (into [:<>]
          (for [{:keys [id type hide-author as-reference]} things]
            (case type
              :rooturl
              (if as-reference
                [display-thing
                 (tb/reference-tb-stuff id db)
                 :truncate short
                 :fields (if short
                           [:headline]
                           [:headline :warstats])]
                [display-thing
                 (tb/root-tb-stuff id db)
                 :truncate short
                 :fields (if short
                           [:headline]
                           [:headline :warstats :reply-count])])
              :opinion
              (let [tbstuff (tb/opinion-tb-stuff id db)]
                [display-thing
                 tbstuff
                 :truncate short
                 :fields (into [:icon] (if short
                                         (if hide-author [:headline] [:author-long])
                                         [:flag-name :date-stamp :author-long :headline]))])
              :author
              [display-thing
               (tb/author-tb-stuff id db)
               :truncate short
               :fields [:author-long]])))))

(defn thing-lister [key]
  (let [spec @(rf/subscribe [:server-parameters key])]
    [thing-displayer (:things2 spec) :trim (:trim spec)]))

(defn process-things [things]
  ;;FIXME: what is needed?
  (println things)
  things)

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
                [:dispatch [:mount-registered key]])})))
