(ns flaglib2.fabricate
  (:require
   [re-frame.core :as rf]

   [flaglib2.fetchers :as fetchers]
   [flaglib2.misc :as misc]
   [flaglib2.urlgrab :as ug]
   [flaglib2.excerpts :as exc]))

(def fabricate-hooks
  {:flaglib2.fetchers/received-author-urls [::get-stuff-for-author-urls]})


(rf/reg-sub ::review-text :-> ::review-text)

(rf/reg-sub
 ::existing-text
 (fn [db _]
   (when-let [target (ug/selected-url-from-db [::specify-target] db)]
     (get-in db [:text-store target :text]))))

(rf/reg-sub
 ::existing-title
 (fn [db _]
   (when-let [target (ug/selected-url-from-db [::specify-target] db)]
     (get-in db [:title-store target :title]))))

(rf/reg-sub
 ::active-text
 ;;FIXME: could consider caching a tdat?
 :<- [::supplied-text]
 :<- [::existing-text]
 (fn [[supplied existing] _]
   (or supplied existing)))

(rf/reg-sub
 ::active-tdat
 :<- [::active-text]
 (fn [[text] _]
   (when text (exc/create-textdata text))))

(rf/reg-sub ::flag :-> ::flag)
(rf/reg-sub
 ::flag-or-default
 :<- [::flag]
 :<- [:server-parameters]
 (fn [[flag params] _]
   (or flag (:flag params))))

(rf/reg-sub ::excerpt :-> ::excerpt)
(rf/reg-sub
 ::excerpt-or-default
 :<- [::excerpt]
 :<- [:server-parameters]
 (fn [[excerpt params] _]
   (or excerpt [(get params :excerpt "") (get params :offset nil)])))

(rf/reg-sub ::excerpt-start :-> ::excerpt-start)
(rf/reg-sub ::excerpt-search :-> ::excerpt-search)

;;FIXME: do we need to factor in current entry of excerpt-search?
(rf/reg-sub
 ::excerpt-found?
 :<- [::excerpt-or-default]
 :<- [::active-tdat]
 (fn [[[excerpt offset] tdat] _]
   (and tdat (exc/find-excerpt-position tdat excerpt :offset offset))))

;;FIXME: also should be handling recommended opinions
(rf/reg-event-fx
 ::get-stuff-for-author-urls
 ;;We assume that author-urls are already in ipfs. No check.
 (fn [{:keys [db]} _]
   {:dispatch [:load-rooturls
               (misc/reformat-urls-lists-simple (list (:flaglib2.fetchers/author-urls db)))
               :no-text true :no-references true]}))


(rf/reg-event-db
 ::reset-supplied-tt
 (fn [db _]
   (let [target (ug/selected-url-from-db [::specify-target] db)]
     (assoc db
            ::supplied-text (get-in db [:text-store target :text] "")
            ::supplied-title (get-in db [:title-store target :title] "")))))

(rf/reg-event-db
 ::set-supplied-text
 (fn [db [_ text]]
   ;;FIXME: Perhaps some processing on text?
   (assoc db ::supplied-text text)))

(rf/reg-event-db
 ::set-supplied-title
 (fn [db [_ title]]
   (assoc db ::supplied-title title)))

(rf/reg-sub ::supplied-text :-> ::supplied-text)
(rf/reg-sub ::supplied-title :-> ::supplied-title)

(rf/reg-sub
 ::text-supplied
 :<- [::supplied-text]
 :<- [::existing-text]
 (fn [[supplied existing] _]
   (if (not-empty supplied)
     (if (= supplied existing)
       false
       true)
     false)))

(rf/reg-sub
 ::title-supplied
 :<- [::supplied-title]
 :<- [::existing-title]
 (fn [[supplied existing] _]
   (if (not-empty supplied)
     (if (= supplied existing)
       false
       true)
     false)))

(rf/reg-event-db
 ::set-flag
 (fn [db [_ flag]]
   (assoc db ::flag flag)))

(rf/reg-event-db
 ::set-excerpt
 (fn [db [_ [excerpt offset]]]
   (assoc db ::excerpt [excerpt offset])))

(rf/reg-event-db
 ::set-comment
 (fn [db [_ comment]]
   (assoc db ::comment comment)))

(rf/reg-sub ::comment :-> ::comment)

(rf/reg-sub
 :current-opinion
 :<- [::flag-or-default]
 :<- [::excerpt-or-default]
 :<- [:selected-url [::specify-target]]
 :<- [:selected-url [::specify-reference]]
 :<- [::comment]
 :<- [::supplied-text]
 :<- [::supplied-title]
 :<- [::text-supplied]
 :<- [::title-supplied]
 (fn [[flag [excerpt offset] target reference comment supplied-text supplied-title text? title?] _]
   (merge
    {:opinion {:target target
               :flag flag
               :excerpt excerpt
               :excerpt-offset offset
               :reference reference
               :comment comment}}
    (when text? {:alternate supplied-text})
    (when title? {:alt-title supplied-title}))))

