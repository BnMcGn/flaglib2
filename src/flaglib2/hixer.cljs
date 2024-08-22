(ns flaglib2.hixer
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.displayables :as disp]))

;; Hixer: hiccup displayer
;;
;; Load and display articles represented in hiccup format, probably from IPFS

;; Begin check-hiccup stuff

(defn check-contents [contents]
  (when-not (or (nil? contents) (seq contents))
    (throw (js/Error. "Needs to be a sequence type")))
  (filter (fn [itm]
          (cond (string? itm) false
                (vector? itm) true
                :else (throw (js/Error. "Only strings and vectors allowed in element body"))))
          (seq contents)))

;;Throw error if problem found, return list of subelements otherwise
(defmulti check-element (fn [[el & _]]
                          (or (#{:a} el)
                              (type el))))

(defmethod check-element cljs.core/Keyword [[el & contents]]
  (when-not (#{:a :div :span :p :h1 :h2 :h3 :h4 :h5 :h6} el)
    (throw (js/Error. "Not a known element type")))
  (check-contents contents))

(defmethod check-element cljs.core/Symbol [[el & contents]]
  (when-not (#{'disp/thread-opinion} el)
    (throw (js/Error. "Not a known display component")))
  ;;FIXME: Need specific checks
  (check-contents contents))

(defmethod check-element :a [[el & contents]]
  (let [[attrib & contents] (if (map? (first contents)) contents (into [nil] contents))]
    (when attrib
      (cond
        (empty? attrib) nil
        (> 1 (count attrib))
        (throw (js/Error. "Only :href attrib allowed in [:a ]"))
        (not (:href attrib))
        (throw (js/Error. "Only :href attrib allowed in [:a ]"))
        (string? (:href attrib)) nil
        :else
        (throw (js/Error. ":href attrib must be a string"))))
    (check-contents contents)))

(defmethod check-element :default [x]
  (throw (js/Error. "Not a known keyword or component")))

(defn check-hiccup [hic]
  (loop [work (list
               (cond (symbol? (first hic)) (check-element hic)
                     (keyword? (first hic)) (check-element hic)
                     :else (check-contents hic)))]
    (when-not (empty? work)
      (if (empty? (first work))
        (recur (rest work))
        (let [res (check-element (first (first work)))]
          (recur
           (if (empty? res)
             (cons (rest (first work)) (rest work))
             (list* res (rest (first work)) (rest work)))))))))

