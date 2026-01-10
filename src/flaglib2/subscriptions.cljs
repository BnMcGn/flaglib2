(ns flaglib2.subscriptions
  (:require
   [re-frame.alpha :as rf]
   [flaglib2.misc :as misc]
   [flaglib2.userfig :as userfig]

   [clojure.walk :as walk]))


(rf/reg-sub
 :server-parameters
 (fn [db [_ key]]
   (get-in db [:server-parameters (or key :default)])))

(rf/reg-sub
 :root-element
 (fn [db _]
   (:root-element db)))

(rf/reg-sub
 :flaglib2.fetchers/author-urls
 (fn [db _]
   (:flaglib2.fetchers/author-urls db)))

(rf/reg-sub
 :advanced-options
 (fn [db _]
   (get-in db [:userfig/user-info :advanced])))

(rf/reg-sub
 :warstats-store
 (fn [db [_ key]]
   (if key
     (get-in db [:warstats-store key])
     (:warstats-store db))))

(rf/reg-sub
 :text-store
 (fn [db [_ key]]
   (if key
     (get-in db [:text-store key])
     (:text-store db))))

(rf/reg-sub
 :title-store
 (fn [db [_ key]]
   (if key
     (get-in db [:title-store key])
     (:title-store db))))

(rf/reg-sub
 :opinion-store
 (fn [db [_ key]]
   (if key
     (get-in db [:opinion-store key])
     (:opinion-store db))))

(rf/reg-sub
 :opinion-tree-store
 (fn [db [_ key]]
   (if key
     (get-in db [:opinion-tree-store key])
     (:opinion-tree-store db))))

(rf/reg-sub
 :hiccup-store
 (fn [db [_ key]]
   (if key
     (get-in db [:hiccup-store key])
     (:hiccup-store db))))

(rf/reg-sub
 :text-status
 (fn [db [_ key]]
   (get-in db [:text-status key])))

(rf/reg-sub
 :references
 (fn [db [_ key]]
   (if key
     (get-in db [:references key])
     (:references db))))

(rf/reg-sub
 :reference-opinions
 (fn [db [_ key]]
   (if key
     (get-in db [:reference-opinions key])
     (:reference-opinions db))))

(rf/reg-sub
 :refd
 (fn [db [_ key]]
   (if key
     (get-in db [:refd key])
     (:refd db))))

(defn proper-text [db key]
  (or (if (misc/iid? key)
        (get-in db [:opinion-store key :clean-comment])
        (get-in db [:text-store key :text]))
      ""))

(rf/reg-sub
 :proper-text
 (fn [db [_ key]] (proper-text db key)))

(defn proper-title [db key]
  (let [attempt (get-in db [:title-store key :text])]
    (or (when (and (misc/iid? key) (not attempt))
          (get-in db [:opinion-store key :clean-comment]))
        "")))

(rf/reg-sub
 :proper-title
 (fn [db [_ key]] (proper-title db key)))

(rf/reg-sub
 :core-db
 :<- [:warstats-store]
 :<- [:text-store]
 :<- [:title-store]
 :<- [:opinion-store]
 :<- [:opinion-tree-store]
 :<- [:references]
 :<- [:refd]
 (fn [[ws txt titl op optree ref refd] _]
   {:warstats-store ws
    :text-store txt
    :title-store titl
    :opinion-store op
    :opinion-tree-store optree
    :references ref
    :refd refd}))


(rf/reg-sub :sub-tree misc/get-sub-tree)

;; :immediate-children now only returns non-tt opinions. Might need to fix this when
;; implementing title/text discussions beyond supply. YAGNI for now

(defn immediate-children [db key]
  (filter
   #(misc/opinion-not-tt? (get-in db [:opinion-store %1]))
   (map first (misc/get-sub-tree db [nil key]))))
(rf/reg-sub :immediate-children :=> immediate-children)

(rf/reg-sub
 :text-tree
 :<- [:core-db]
 (fn [db [_ key]]
   (let [subtree (misc/get-sub-tree db [nil key])]
     (filter #(misc/opinion-targets-text? (get-in db [:opinion-store (first %1)])) subtree))))

(rf/reg-sub
 :title-tree
 :<- [:core-db]
 (fn [db [_ key]]
   (let [subtree (misc/get-sub-tree db [nil key])]
     (filter #(misc/opinion-targets-title? (get-in db [:opinion-store (first %1)])) subtree))))

(rf/reg-sub
 :normal-tree
 :<- [:core-db]
 (fn [db [_ key]]
   (let [subtree (misc/get-sub-tree db [nil key])]
     (filter #(misc/opinion-not-tt? (get-in db [:opinion-store (first %1)])) subtree))))

(rf/reg-sub
 :visible-tree
 :<-[:core-db]
 :<-[:visibility]
 (fn [[db vis] [_ key]]
   (let [subtree (misc/get-sub-tree db [nil key])]
     (defn visible? [itm]
       (let [v (get vis itm)]
         (= :show (:list-display v))))
     (walk/postwalk (fn [x] (if (string? x) (if (visible? x) x nil) x)) subtree))))

(rf/reg-sub
 :invisible-tree
 :<-[:core-db]
 :<-[:visibility]
 (fn [[db vis] [_ key]]
   (let [subtree (misc/get-sub-tree db [nil key])]
     (defn is-visible? [itm]
       (let [v (get vis itm)]
         (= :show (:list-display v))))
     (walk/postwalk (fn [x] (if (string? x) (if (is-visible? x) nil x) x)) subtree))))

(defn target-decision [{:keys [warstats-store text-store text-status]} target]
  (let [text (get text-store target)
        status (get text-status target)
        warstat (get warstats-store target)
        have-text (and text (:text text))
        responses (and warstat (not (zero? (:replies-total warstat))))]
    {:status (cond (and responses have-text) :reviewed
                   have-text :available
                   ;;FIXME: Not sure that :wait is correct here. Perhaps add an appropriate message
                   ;; for the NIL circumstance
                   :else (or (keyword (walk/keywordize-keys (:status status))) :wait))
     :message (and status (:message status))}))

(rf/reg-sub
 :target-decision
 (fn [db [_ target]]
   (target-decision db target)))

(rf/reg-sub :window-size :-> :window-size)

(rf/reg-sub
 :window-small?
 :<- [:window-size]
 (fn [ws _]
   (if (= :xs ws) true false)))

;;Returns a working summary of the title status: a vector containing:
;;  [best-guess-at-title was-title-found is-title-edited]
(rf/reg-sub
 :title-summary
 (fn [db [_ key]]
   (let [tinfo (get-in db [:title-store key])
         iid (misc/iid? key)
         opinion (and iid (get-in db [:opinion-store key]))]
     (cond
       (misc/has-title? tinfo)
       (if (misc/alternate-title? tinfo)
         [(:title tinfo) true true]
         [(:title tinfo) true false])
       iid
       (if-let [cmt (and opinion (:clean-comment opinion))]
         [cmt true false]
         ;;NOTE: might cause trouble, can return to ""?
         [(misc/entities "&nbsp;") false false])
       :else [(or key "") false false]))))
