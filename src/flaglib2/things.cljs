(ns flaglib2.things
  (:require
   [re-frame.core :as rf]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.stacker :as stack]
   [flaglib2.titlebar :as tb]))

(defn display-thing [tbstuff & {:keys [fields]}]
  (into [:div {:class (misc/class-string (:bg-color tbstuff) "flex flex-row items-center my-1")}]
        (tb/assemble-bar-parts tbstuff fields)))

(defn display-thing-short [tbstuff & {:keys [fields]}]
  (let [tbstuff (update tbstuff :headline into [:no-fontsize true])]
    (into [:div {:class (misc/class-string (:bg-color tbstuff) "grid my-1")}]
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
          (for [{:keys [id type hide-author]} things]
            (case type
              :rooturl
              [thing-element
               (tb/root-tb-stuff id db)
               :fields (if short
                         [:headline]
                         [:headline :warstats :reply-count])]
              :reference
              [thing-element
               (tb/reference-tb-stuff id db)
               :fields (if short
                         [:headline]
                         [:headline :warstats])]
              :opinion
              (let [tbstuff (tb/opinion-tb-stuff id db)]
                [(if short display-thing-opinion-short display-thing-opinion)
                 tbstuff
                 :fields (into [:opinion-icon]
                               (if short
                                 (if hide-author [:headline] [:author-long])
                                 [:flag-name :date-stamp :author-long :headline]))])
              :question
              [thing-element
               (tb/question-tb-stuff id db)
               :fields (if short
                         [:headline]
                         [:headline :warstats])]
              :author
              [thing-element
               (tb/author-tb-stuff id db)
               :fields [:author-long]])))))

(defn thing-lister [key]
  (let [spec @(rf/subscribe [:thing-lister key])]
    [thing-displayer (:things2 spec) :trim (:trim spec)]))

(defn process-things [things]
  (map #(update %1 :type keyword) things))

(defn thing-loaders [things]
  (into []
        (for [{:keys [type id]} things
              :when (#{:rooturl :opinion :question} type)]
          (case type
            :rooturl
            [:dispatch [:load-rooturl id :no-text true :no-references true]]
            :opinion
            [:dispatch [:load-opinion id]]
            :question
            [:dispatch [:load-opinion id]]
            :author
            nil ;May need later
            ))))

;;Decides if a thing lister needs more items.
;; things1: pre-processed unfiltered source
;; things2: filtered things
;; notthings: filter rejects
;;Problem: we can't know if something matches if we don't have its warstats, etc loaded. So
;;sometimes we return a maybe.
(defn need-more? [thingplace]
  (let [{:keys [things1 things2 notthings needed finished]} thingplace]
    (cond
      finished false
      (<= needed (count things2)) false
      (> needed (count things1)) true
      (= (+ (count things2) (count notthings)) (count things1)) true
      :else :maybe)))

(defn new-things [thingplace things]
  (let [things (process-things things)]
    [(update thingplace :things1 into things)
     (thing-loaders things)]))

(rf/reg-event-db
 ::filter-things
 (fn [db [_ key]]
   (let [thingspec (get-in db [::thing-listers key])
         filter (:filter thingspec)
         ])))

(rf/reg-sub
 :thing-lister
 (fn [db [_ key]]
   (if key (get-in db [:thing-listers key])
       (:thing-listers db))))

(rf/reg-event-fx
 :thing-lister
 (fn [{:keys [db]} [_ key]]
   (let [spec (get-in db [:server-parameters key])
         loc [:thing-listers key]
         {:keys [url things]} spec
         things (if url things (process-things things))
         spec (cond-> spec
                (not url) (assoc :things2 things))
         out {:db (-> db
                      (assoc-in loc spec)
                      ;;FIXME: What when it isn't a side element?
                      (assoc-in [:server-parameters key :side-element] thing-lister))
              :fx [[:dispatch [:mount-registered key]]]}
         out (cond-> out
               (:url spec) (misc/prepend-dispatch [:add-after-hooks
                                                   {:flaglib2.ipfs/complete-debounce
                                                    [::filter-things key]}]))
         prepender (fn [fx dispatches] (apply misc/prepend-dispatch fx dispatches))]
     ;;url indicates that we have an "infinite" source
     (cond-> out
       url (misc/prepend-dispatch [::stack/init loc
                                   {::stack/url url
                                    ::stack/process-chunk process-things
                                    ::stack/chunk things}])
       (not url) (prepender (thing-loaders things))))))
