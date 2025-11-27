(ns flaglib2.visibility
  (:require
   [cljs-time.core :as tm]
   [re-frame.alpha :as rf]
   [clojure.string :as string]

   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]
   [flaglib2.flags :as flags]
   [flaglib2.deco :as deco]
   ))

;; Mechanical: opinions that should optionally be omitted from some displays, such as lists,
;; because they don't have content. Votes, dircs, etc.
;; In some sense, text/title should be included in this, but we separate them out because
;; distinctions will be necessary in text/title threads
(defn is-mechanical? [opinion]
  (and
   (misc/opinion-not-tt? opinion)
   (not (misc/opinion-has-comment? opinion))))

;;NOTE: We are not making age decisions here.
;;FIXME: Will :fade3 work? Might do better to use curation...
(defn base-fade-info [db id]
  "Decide if an item should be dropped from lists and threads. Returns the following recommendations:
:show - default
:mechanical - is a contentless flag, dirc, marker excerpt, or maybe hashtag
:fade1 - junk
:fade2 - doubtful
:fade3 - nonessential"
  (let [warstats (get-in db [:warstats-store id])
        word (mood/in-a-word warstats)
        mechanical (when (misc/iid? id)
                     (is-mechanical? (get-in db [:opinion-store id])))]
    (cond
      (#{:negative} word) :fade1
      mechanical :mechanical
      (#{:ignored} word) :fade2
      (#{:unsupported :sidelined :contested} word) :fade3
      :else :show)))

;;Add a time delay to some of the base items.
(defn list-display-policy [db id]
  (let [base (base-fade-info db id)
        warstats (get-in db [:warstats-store id])]
    (if (:tt-thread warstats)
      :text-title
      (case base
        :fade1
        (let [freshness (:tree-freshness warstats)]
          (if (> (misc/milliseconds-ago freshness) (* 3 misc/ms-month))
            :faded
            :show))
        :fade2 :show
        :fade3 :show
        base))))

;;Because sometimes we need a flagset for a hilite
(defn flagset-from-multiple [db ids]
  (let [oflags (into #{} flags/other-flags)]
    (reduce (fn [m [k v]] (update m k (fnil + 0) v)) {}
            (for [id ids
                  :let [opinion (get-in db [:opinion-store id])
                        flag (oflags (:flag opinion))
                        score (when flag (get-in db [:warstats-store id :effect]))]
                  :when flag]
              [flag score]))))

;;Multiplier to adjust likelyhood of concealment
(def concealables {:negative-inflammatory 0.5
                   :negative-disturbing 1.0
                   :negative-out-of-bounds 1.0})

(defn warn-off? [flagset concealables]
  (sort-by second >
           (for [[flag effect] (seq flagset)
                 :let [mult (get concealables flag)]
                 :when mult]
             [flag (* mult effect)])))

;This only has to handle things that are visible, but need a warn off.
(defn list-item-display-policy [db id]
  (let [warstats (get-in db [:warstats-store id])
        word (mood/in-a-word warstats)]
    (when (#{:restricted :contested} word)
      (warn-off? warstats concealables))))


;; Other categories where policies/warn-offs will be needed:
;; - titles
;; - usernames/avatars
;; - article texts (copyright)

;; Find out if an ancestor has caused this portion of tree to be in a sidelined state
(defn- sidelineage [vstore tree-address]
  (loop [parents (reverse (butlast tree-address))]
    (when-not (empty? parents)
      (let [curr (first parents)
            vis (get vstore curr)]
        (if (= :sidelined (get vis :word))
          curr
          (recur (rest parents)))))))

(defn update-ancestral-info [opinion-store visibility-store iid]
  (if (misc/iid? iid)
    (let [opinion (get opinion-store iid)
          vis (get visibility-store iid)
          ta (:tree-address opinion)]
      (if (and vis ta)
        (let [parid (:target opinion)
              parvis (get visibility-store parid)
              slin (sidelineage visibility-store ta)]
          (cond-> vis
            true (assoc :parent-list-display (:list-display parvis))
            slin (assoc :sidelined-by slin)))
        vis))
    (get visibility-store iid)))

(rf/reg-flow
 {:id :visibility
  :inputs {:wstore [:warstats-store]
           :ostore [:opinion-store]}
  :output
  (fn [{:keys [wstore ostore]}]
    (let [db {:opinion-store ostore :warstats-store wstore}
          vis (into {}
                    (for [id (keys wstore)]
                      [id
                       {:list-display (list-display-policy db id)
                        :warn-off (list-item-display-policy db id)
                        :word (mood/in-a-word (get wstore id) :key id :db db)}]))]
      (into {}
            (for [id (keys vis)]
              [id (update-ancestral-info ostore vis id)]))))
  :path [:visibility]})

(rf/reg-sub
 :visibility
 (fn [db [_ key]]
   (if key
     (get-in db [:visibility key])
     (:visibility db))))

(defn warn-off-style [flag]
  (merge (deco/warn-off-stripes flag)
         {:color "white"
          :border-color "#444"}))

;;FIXME: Move to things?
(defn line-warn-off [id]
  (let [vis @(rf/subscribe [:visibility id])
        flag (first (first (:warn-off vis)))
        flagfo (get flags/flags flag)
        color (:color flagfo)]
    [:a {:href (if (misc/iid? id)
                 (misc/make-opinion-url {:iid id})
                 (misc/make-target-url id))}
     [:h4
      {:style (warn-off-style flag)
       :class "border-[3px] pl-6"}
      (:label flagfo)]]))

