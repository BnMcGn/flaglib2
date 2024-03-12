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

(defn flavor-from-own-warstats [warstats]
  (let [effect (:effect warstats)
        controv (:controv warstats)
        ;;FIXME: pos is unused. Should it be used?
        pos (+ (:x-right warstats) (:x-up warstats))
        neg (+ (:x-wrong warstats) (:x-down warstats))
        ;; flags may eventually be put in own object, but for now...
        flags warstats
        ;;FIXME: Need clearer indicators and more nuance for handling other flags. Not too bad because
        ;; it just contributes to flavor. Will need visibility system to do this right.
        badness (reduce + (map #(get flags % 0) badflags))
        goodness (reduce + (map #(get flags % 0) goodflags))
        diff (misc/relative-to-range 0 effect controv)]
    (if (< 0 (+ effect goodness))
      (if (> diff 0.7)
        :contested
        (if (and (> 10 effect) (< 0 badness))
          :contested
          :positive))
      (if (< 0 neg)
        :negative
        :neutral))))

(defn magnitude [item & {:keys [keyfunc] :or {keyfunc identity}}]
  (let [val (keyfunc item)]
    (cond
      (< 200 val) 4
      (< 50 val) 3
      (< 10 val) 2
      (< 3 val) 1
      :else 0)))
