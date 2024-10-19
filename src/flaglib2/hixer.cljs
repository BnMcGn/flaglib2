(ns flaglib2.hixer
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [cljs.reader]
   [ajax.core :as ajax]
   [clojure.walk :as walk]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs :as ipfs]
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
  (when-not (#{'flaglib2.displayables/thread-opinion
               'flaglib2.hixer/embedded-opinion
               'flaglib2.hixer/embedded-article} el)
    (throw (js/Error. "Not a known display component")))
  ;;FIXME: Need specific checks
  ;(check-contents contents)
  )

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

(defn embedded-opinion [& {:keys [iid]}]
  [:div {:class "bg-slate-100 p-3 sm:mx-3"}
   [disp/opinion-info iid :show-excerpt true]])

(defn embedded-article [& {:keys [url]}]
  [:div {:class "bg-slate-100 p-3 sm:mx-3"}
   [disp/root-title :url url]])

(defn process-hiccup [hic]
  (walk/postwalk-replace
   {'flaglib2.displayables/thread-opinion disp/thread-opinion
    'flaglib2.hixer/embedded-article embedded-article
    'flaglib2.hixer/embedded-opinion embedded-opinion}
   hic))

(defn extract-hiccup-ids [hiccup]
  (loop [stuff (flatten hiccup)
         accum '()]
    (case (first stuff)
      nil (reverse accum)
      :iid (recur (rest (rest stuff)) (conj accum [:dispatch [:load-opinion (second stuff)]]))
      :url (recur (rest (rest stuff)) (conj accum [:dispatch [:load-rooturl (second stuff)]]))
      (recur (rest stuff) accum))))

(rf/reg-event-fx
 ::request-opinion-hiccup
 (fn [{:keys [db]} [_ iid]]
   {:http-xhrio {:method :get
                 :uri (str "/ipns/" (:ipns-host db) "/opinions/" iid "/hiccup.edn")
                 :timeout 18000
                 :response-format (ajax/text-response-format)
                 :on-success [::received-opinion-hiccup iid]}}))

(rf/reg-event-fx
 ::received-opinion-hiccup
 (fn [{:keys [db]} [_ key result]]
   (let [hiccup (cljs.reader/read-string result)
         pass? (try
                 (check-hiccup hiccup)
                 true
                 (catch js/Error e
                   (println "Error loading hiccup:")
                   (throw e)
                   ;;Don't store on error
                   false))]
     {:db (if pass? (assoc-in db [:hiccup-store key] hiccup) db)
      :fx (into [] (when pass? (extract-hiccup-ids hiccup)))})))


(defn opinion-hiccup [iid]
  (let [hic @(rf/subscribe [:hiccup-store iid])]
    (into [:div
           {:class "m-1 mt-4 child-div:my-5"}]
          ;;Drop initial :div
          (rest (process-hiccup hic)))))
