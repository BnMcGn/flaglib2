(ns flaglib2.mood
  (:require
   [cljs-time.core :as tm]

   [flaglib2.misc :as misc]
   [flaglib2.deco :as deco]))



(defn freshness-from-warstats [warstats-coll]
  (let [now (tm/now)
        old (tm/minus- now (tm/days 2))
        recent (tm/minus- now (tm/hours 1))
        newest (when-not (empty? warstats-coll)
                 (tm/latest (map (fn [warstats] (:tree-freshness warstats)) warstats-coll)))]
    (cond (not newest) "old"
          (tm/before? newest old) "old"
          (tm/before? newest recent) "recent"
          :else "new")))

(defn- significant
  "is n1 significant related to n2"
  [n1 n2]
  (when (< 0 n1)
    (if (>= n1 n2)
      true
      (if (< 0.7 (misc/relative-to-range 0 n2 n1))
        true
        false))))

(def badflags #{:negative-spam :negative-inflammatory :negative-language-warning :negative-disturbing
                :negative-logical-fallacy :negative-out-of-bounds :custodial-redundant
                :custodial-out-of-date :custodial-retraction :custodial-incorrect-flag
                :custodial-flag-abuse :custodial-offtopic :custodial-arcane})
(def goodflags #{:positive-interesting :positive-funny})

(defn flavor-from-multiple
  "Creates a flavor descriptor from a collation of all the warstats passed in. Needs to handle multiple warstats collections because it is used for excerpts which may represent multiple opinions."
  [db ids]
        ;;; controv: The opinions are themselves contested
        ;;; positive: Relatively uncontested postive opinions
        ;;; negative: Relatively uncontested negative opinions
        ;;; We return contested if there are significant values in both positive and negative
        ;;; We return contested if controv is significant
        ;;; Neutral if none of the three are significant.
  (defn warstat-direction [opid]
    (let [opinion (get-in db [:opinion-store opid])
          ws (get-in db [:warstats-store opid])]
      (cond
        (significant (:controversy ws) (:effect ws)) :controv
        (= "pro" (:direction ws)) :positive
        (= "con" (:direction ws)) :negative
        (contains? badflags (:flag opinion)) :negative
        (contains? goodflags (:flag opinion)) :positive
        :else :none)))
  (let [{:keys [controv positive negative]}
        (reduce #(update %1 %2 inc) (cons {} (map warstat-direction ids)))
        top (max controv positive negative)]
    (cond
      (> 0.5 top) :neutral
      (significant controv top) :contested
      (= top positive)
      (if (significant negative positive)
        :contested
        :positive)
      (= top negative)
      (if (significant positive negative)
        :contested
        :negative))))

(defn flavor+freshness [db ids]
  ;;(str (name (flavor-from-warstats wcoll)) "-" (freshness-from-warstats wcoll))
  ;; ignore freshness, use tailwind stuff
  (get deco/flavor-background (flavor-from-multiple db ids)))


(defn magnitude [item & {:keys [keyfunc] :or {keyfunc identity}}]
  (let [val (keyfunc item)]
    (cond
      (< 200 val) 4
      (< 50 val) 3
      (< 10 val) 2
      (< 3 val) 1
      :else 0)))

;;A more detailed version of flavor-from-own-warstats
;;FIXME: would like :ambivalent, but need own contrary votes, excerpt score separated out,
;; perhaps faction info to see if faction is internally ambivalent. Not easy now!
;;FIXME: Would like info on participants in this conversation
(defn in-a-word [warstats & {:keys [key db]}]
  (let [flags warstats
        goodness (reduce + (map #(get flags % 0) goodflags))
        badness (reduce + (map #(get flags % 0) badflags))
        {:keys [x-right x-wrong x-up x-down]} warstats
        goodcount (+ goodness x-right x-up)
        badcount (+ badness x-wrong x-down)
        refd (and key db (get-in db [:refd key]))]
    (cond
      (misc/significant-majority? goodcount badcount)
      (if (empty? refd) :positive :significant)
      (misc/significant-majority? badcount goodcount) :negative
      (< 0 (+ goodcount badcount))
      (cond
        ;;Is the badcount coming (mostly) from flags?
        (misc/significant-majority-of? badness badcount)
        (let [{:keys [negative-spam negative-inflammatory negative-language-warning
                      negative-disturbing negative-out-of-bounds custodial-redundant
                      custodial-out-of-date custodial-retraction custodial-flag-abuse
                      custodial-arcane custodial-offtopic]} flags]
          (cond
            (misc/significant-majority-of? (+ negative-spam
                                              negative-disturbing
                                              negative-out-of-bounds)
                                           badcount) :restricted
            (misc/significant-majority-of? (+ custodial-redundant
                                              custodial-out-of-date
                                              custodial-retraction
                                              custodial-offtopic
                                              custodial-arcane)
                                           badcount) :sidelined
            :else :contested))
        (and (misc/significant-majority-of? x-up goodcount)
             (misc/significant-majority-of? x-wrong badcount)) :unsupported
        (and (misc/significant-majority-of? x-right goodcount)
             (misc/significant-majority-of? x-down badcount)) :awkward
        :else :contested)
      :else :ignored)))

(defn flavor-from-own-warstats [warstats]
  (let [word (in-a-word warstats)]
    (case word
      :ignored :neutral
      (:positive :significant) :positive
      :negative :negative
      (:sidelined :restricted :unsupported :awkward :contested) :contested
      :neutral)))
