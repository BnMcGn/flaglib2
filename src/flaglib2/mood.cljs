(ns flaglib2.mood
  (:require
   [cljs-time.core :as tm]
   [clojure.string :as str]

   [flaglib2.misc :as misc]))



(defn freshness-from-warstats [warstats-coll]
  (let [now (tm/now)
        old (tm/minus- now (tm/day 2))
        recent (tm/minus- now (tm/hour 1))
        newest (tm/latest
                (map (fn [warstats] (:tree-freshness warstats)) warstats-coll))]
    (cond (tm/before? newest old) "old"
          (tm/before? newest recent) "recent"
          :else "new")))

(defn- significant [n1 n2]
  "is n1 significant related to n2"
  (when (< 0 n1)
    (if (>= n1 n2)
      true
      (if (< 0.7 (misc/relative-to-range 0 n2 n1))
        true
        false))))

(defn flavor-from-warstats [warstats-coll]
  "Creates a flavor descriptor from a collation of all the warstats passed in. Needs to handle multiple warstats collections because it is used for excerpts which may represent multiple opinions."
        ;;; controv: The opinions are themselves contested
        ;;; positive: Relatively uncontested postive opinions
        ;;; negative: Relatively uncontested negative opinions
        ;;; We return contested if there are significant values in both positive and negative
        ;;; We return contested if controv is significant
        ;;; Neutral if none of the three are significant.
  (defn warstat-direction [ws]
    (cond
      (significant (:controversy ws) (:effect ws)) :controv
      (= "pro" (:direction ws)) :positive
      (= "con" (:direction ws)) :negative
      :else :none))
  (let [{:keys [controv positive negative]}
        (reduce #(update %1 %2 inc) (cons {} (map warstat-direction warstats-coll)))
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

(defn flavor+freshness [warstats & ids]
  (let [wcoll
        (into {}
              (for [id ids
                    ws (get warstats id)
                    :when ws]
                [id ws]))]
    (str (name (flavor-from-warstats wcoll)) "-" (freshness-from-warstats wcoll))))




(defn magnitude [item & {:keys [keyfunc] :or {:keyfunc identity}}]
  (let [val (keyfunc item)]
    (cond
      (< 200 val) 4
      (< 50 val) 3
      (< 10 val) 2
      (< 3 val) 1
      :else 0)))
