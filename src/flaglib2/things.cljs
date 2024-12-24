(ns flaglib2.things
  (:require
   [re-frame.core :as rf]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs :as ipfs]
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

(defn current-things [spec]
  (let [{:keys [filter things1 ::stack/stack things2]} spec]
    (if filter
      things2
      (or things1 stack))))

(defn thing-lister [key]
  (let [spec @(rf/subscribe [:thing-lister key])]
    [thing-displayer (current-things spec) :trim (:trim spec)]))

(defn process-things [things]
  (map #(update %1 :type keyword) things))

(defn thing-loaders [things]
  (into []
        (for [{:keys [type id]} things
              :when (#{:rooturl :opinion :question} type)]
          (case type
            :rooturl
            [:load-rooturl id :no-text true :no-references true]
            :opinion
            [:load-opinion id]
            :question
            [:load-opinion id]
            :author
            nil ;May need later
            ))))

;;Decides if a thing lister needs more items.
;; things1: pre-processed unfiltered source
;; things2: filtered things
;; notthings: filter rejects
;;Problem: we can't know if something matches if we don't have its warstats, etc loaded. So
;;sometimes we return a maybe.
(defn need-more? [spec]
  (let [{:keys [things1 things2 filterable needed finished]} spec]
    (cond
      finished false
      (<= needed (count things2)) false
      (> needed (count things1)) true
      ;;Don't decide to need more before warstats have had a chance to load
      (= (count filterable) (count things1)) true
      :else :maybe)))

(rf/reg-event-fx
 ::filter-things
 (fn [{:keys [db]} [_ key]]
   (let [spec (get-in db [:thing-listers key])
         things (or (::stack/stack spec) (:things1 spec))
         filterable (filter #((:warstats-store db) (:id %)) things)
         things2 ((:filter spec) db filterable)
         newdb {:db (assoc-in db [:thing-listers key :things2] things2)}
         need-more (need-more? (assoc spec :things2 things2 :filterable filterable))]
     (cond-> newdb
       need-more (misc/append-dispatch [::stack/request-chunk [:thing-listers key]])))))

(rf/reg-event-fx
 ::on-thing-chunk
 (fn [{:keys [db]} [_ key chunk]]
   (let [loc [:thing-listers key]
         spec (get-in db loc)
         limit (::stack/limit spec)]
     (apply
      misc/append-dispatch
      {:db (cond-> db
             (< (count chunk) limit) (assoc-in (into loc [:finished]) true))}
      (thing-loaders chunk)))))

(rf/reg-sub
 :thing-lister
 (fn [db [_ key]]
   (if key (get-in db [:thing-listers key])
       (:thing-listers db))))

(def settings
  {"author-open-questions"
   {:filter (fn [db things]
              (for [{:keys [type id]:as th} things
                    :when (not (misc/is-answered? (get-in db [:warstats-store id])))]
                th))}})

(rf/reg-event-fx
 :thing-lister
 (fn [{:keys [db]} [_ key]]
   (let [spec (get-in db [:server-parameters key])
         loc [:thing-listers key]
         {:keys [url things]} spec
         spec (merge spec (get settings (:name spec)))
         things (if url things (process-things things))
         spec (cond-> spec
                (not url) (assoc :things1 things))
         out {:db (-> db
                      (assoc-in loc spec)
                      ;;FIXME: What when it isn't a side element?
                      (assoc-in [:server-parameters key :side-element] thing-lister))
              :fx [[:dispatch [:mount-registered key]]]}
         out (cond-> out
               (:filter spec) (misc/prepend-dispatch [:add-after-hooks
                                                   {::ipfs/complete-debounce
                                                    [::filter-things key]}]))
         prepender (fn [fx dispatches] (apply misc/prepend-dispatch fx dispatches))]
     ;;url indicates that we have an "infinite" source
     (cond-> out
       url (misc/prepend-dispatch [::stack/init loc
                                   {::stack/url url
                                    ::stack/process-chunk process-things
                                    ::stack/chunk things
                                    ::stack/on-chunk [::on-thing-chunk key]}])
       (not url) (prepender (thing-loaders things))))))
