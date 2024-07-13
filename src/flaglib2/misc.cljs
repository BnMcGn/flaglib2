(ns flaglib2.misc
  (:require
   [cljs-time.format]
   [cljs-time.coerce :as timec]
   [cljs-time.core :as time]
   [reagent.dom.server :refer [render-to-string]]
   [re-frame.registrar]
   [re-frame.core :as rf]
   [re-frame.db]
   [clojure.string :as string]
   [goog.string :as gstring]
   [goog.object :as go]))

;; Opinion url recognizer

(def ipfs-hash-pattern #"baf[a-z0-9]{56}")

(defn iid? [item]
  (when (string? item) (re-matches ipfs-hash-pattern item)))


(defn make-opinion-url [opinion]
  (str "/o/" (:iid opinion)))

(defn make-author-url [author]
  (str "/u/" (js/encodeURIComponent author)))

(defn make-target-url [identifier]
  (str "/target/" (js/encodeURIComponent identifier)))

(defn excerpt-reply-link [target excerpt]
  (let [exstr (when excerpt (str "&excerpt=" (js/encodeURIComponent excerpt)))]
    (str "/opinion/?target=" (js/encodeURIComponent target) exstr)))

(defn encode-uri-component2 [uri]
  (let [ichars ":\",()/\\%?="]
    (apply str
           (map (fn [itm]
                  (if (= -1 (.indexOf ichars itm))
                    itm
                    (str "%" (.toUpperCase (.toString (.charCodeAt itm 0) 16)))))
                (seq uri)))))


(defn username []
  (go/get js/window "USERNAME"))

(defn make-login-url []
  ;;FIXME: shouldn't be hard coded. Server should set?
  (str "/clath/login/?destination=" (js/encodeURIComponent (.. js/document -location -url))))

;;; Tree tools

(defn opinion? [])

(defn focus? [focus tree-address]
  (or
   (not focus)
   (and (empty? focus) (empty? tree-address))
   (and (not (empty? focus))
        (not (empty? tree-address))
        (= (last focus) (last tree-address)))))

(defn focus-parent? [])

(defn sub-tree [treead optree]
  (if (empty? treead)
    optree
    (recur (rest treead)
           (rest (first (filter #(= (first treead) (first %1)) optree))))))

(defn deep-opinion? [opinion]
  (< 1 (count (:tree-address opinion))))

(defn answer? [opinion]
  (#{:negative-evidence :negative-disagree} (:flag opinion)))

(defn opinion-has-dirc? [opinion dirc]
  (some #(= (first %1) dirc) (:directives opinion)))

(defn opinion-targets-text? [opinion]
  (some (partial opinion-has-dirc? opinion) [:target-text :suggest-target-text]))

(defn opinion-targets-title? [opinion]
  (some (partial opinion-has-dirc? opinion) [:target-title :suggest-target-title]))

(defn opinion-not-tt? [opinion]
  (not (some (partial opinion-has-dirc? opinion)
             [:target-text :suggest-target-text :target-title :suggest-target-title])))

(defn opinion-suggests-tt? [opinion]
  (some (partial opinion-has-dirc? opinion ) [:suggest-target-title :suggest-target-text]))

(defn opinion-supplies-text? [opinion db]
  (let [{:keys [iid target]} opinion
        tinfo (get-in db [:text-store target])]
    (= (:text-source tinfo) iid)))

(defn opinion-supplies-title? [opinion db]
  (let [{:keys [iid target]} opinion
        tinfo (get-in db [:title-store target])]
    (= (:title-source tinfo) iid)))

(defn get-sub-tree [db [_ key]]
  (if (iid? key)
    (let [opinion (get-in db [:opinion-store key])
         optree (when opinion (get-in db [:opinion-tree-store (:rooturl opinion)]))]
     (when (and opinion optree)
       (sub-tree (:tree-address opinion) optree)))
    (get-in db [:opinion-tree-store key])))

;;; Time tools

(def formatter (cljs-time.format/formatters :date-time-no-ms))
(defn parse-time [timestamp]
  (cljs-time.format/parse formatter timestamp))

(def ms-second 1000)
(def ms-minute (* 60 ms-second))
(def ms-hour (* 60 ms-minute))
(def ms-day (* 24 ms-hour))
(def ms-week (* 7 ms-day))
(def ms-month (* 30 ms-day)) ;Ok, things start to get weird.
(def ms-year (* 365 ms-day))

(defn ago [timestamp]
  (let [now (timec/to-long (time/now))
        diff (- now (timec/to-long timestamp))]
    (when (neg? diff)
      (throw (js/Error. "Future date!")))
    (let [[quantity unit]
          (cond
            (> diff (* 2 ms-year)) [(int (/ diff ms-year)) "year"]
            (> diff (* 2 ms-month)) [(int (/ diff ms-month)) "month"]
            (> diff (* 2 ms-week)) [(int (/ diff ms-week)) "week"]
            (> diff (* 2 ms-day)) [(int (/ diff ms-day)) "day"]
            (> diff (* 1 ms-hour)) [(int (/ diff ms-hour)) "hour"]
            (> diff (* 1 ms-minute)) [(int (/ diff ms-minute)) "minute"]
            (> diff (* 1 ms-second)) [(int (/ diff ms-second)) "second"])
          unit (if (> quantity 1) (str unit "s") unit)]
      [quantity unit])))

(def whitespace-characters #{\space \newline \backspace \tab \formfeed \return})

(def url-pattern #"(?i)^(?:(?:https?|ftp)://)(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)(?:\.(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)*(?:\.(?:[a-z\u00a1-\uffff]{2,}))\.?)(?::\d{2,5})?(?:[/?#]\S*)?$")

(defn url? [item]
  (and (string? item) (re-matches url-pattern item)))

(defn url-domain [url]
  (let [domain (-> url
                   (string/split "//")
                   second
                   (string/split "/")
                   first)]
    (if (string/starts-with? domain "www.")
      (subs domain 4)
      domain)))

(defn string= [a b]
  (or
   (= a b)
   (= (name (or a "")) (name (or b "")))))

(defn first-index [itm coll & {:keys [test] :or {test =}}]
  (first (keep-indexed #(when (test %2 itm) %1) coll)))

(defn class-string [& colls]
  (string/join " " (flatten colls)))

(defn entities [itm]
  (gstring/unescapeEntities itm))

(defn pluralize [countable label]
  (let [ct (count countable)]
    (if (= 1 ct)
      label
      (str label "s"))))

;;Debugging tools

(defn say [itm]
  (println itm)
  itm)

(defn say-when [cond itm]
  (when cond (println itm))
  itm)

(defn say-hiccup [itm]
  (println "SOURCE:")
  (println itm)
  (println "HTML:")
  (println (render-to-string itm))
  itm)

(defn dump [itm]
  (set! (. js/window -dumped) itm)
  itm)

(defn dive []
  (. js/window -dumped))

(defn list-events []
  (let [store @re-frame.registrar/kind->id->handler]
    (keys (:event store))))

(defn list-subs []
  (let [store @re-frame.registrar/kind->id->handler]
    (keys (:sub store))))

(defn list-fx []
  (let [store @re-frame.registrar/kind->id->handler]
    (keys (:fx store))))

(defn reframe-db
  "Fetch the reframe db. Used for testing/console. Not for live code!"
  []
  @re-frame.db/app-db)





(defn relative-to-range
  "Returns a value indicating where num is positioned relative to start and end. If num lies between start and end, the return value will be between 0.0 and 1.0."
  [start end num]
  (/ (- num start) (- end start)))

(defn as-in-range
  "Complement of relative-to-range function. Treats num as if it were a fraction of the range specified by start and end. Returns the absolute number that results."
  [start end num]
  (+ start (* num (- end start))))

(defn vast-majority?
  "Is A at least 1000% larger than B?"
  [a b]
  (and (> 0 a)
       (< (relative-to-range 0 a b) 0.1)))

(defn significant-majority?
  "Is B 70% or less of the size of A?"
  [a b]
  (and (> 0 a)
       (< (relative-to-range 0 a b) 0.7)))


;; Title Utilities

(defn has-title? [tinfo]
  (when-let [title (:title tinfo)]
    (when-not (empty? title) title)))

(defn alternate-title? [tinfo]
  (and (has-title? tinfo) (not (= :initial (:title-source tinfo)))))



(defn reformat-urls-lists [lists titles]
  (for [[k urls] (seq lists)
        url (or urls [])
        :let [title (has-title? (get titles url))
              data {:url url :category k}]]
    (if title (assoc data :title title) data)))

(defn reformat-urls-lists-simple [lists]
  (for [l lists
        [_ urls] (or l {})
        url (or urls [])]
    url))

(rf/reg-fx
 :call-something
 (fn [call-info]
   (let [callable (first call-info)]
     (if (fn? callable)
       (apply callable (rest call-info))
       (rf/dispatch call-info)))))


(defn element-ancestors [element]
  (when element
    (lazy-seq (cons element (element-ancestors (.-parentElement element))))))

(defn dump-rect [el]
  (let [rec (.getBoundingClientRect el)]
    {:left (.-left rec) :right (.-right rec) :top (.-top rec) :bottom (.-bottom rec)}))

(defn position-difference [element1 element2]
  (let [pos1 (. element1 (getBoundingClientRect))
        pos2 (. element2 (getBoundingClientRect))]
    {:top (- (. pos1 -top) (. pos2 -top))
     :left (- (. pos1 -left) (. pos2 -left))
     :bottom (- (. pos1 -bottom) (. pos2 -bottom))
     :right (- (. pos1 -right) (. pos2 -right))}))

(defmacro when-let*
          [bindings & body]
          `(let ~bindings
                (if (and ~@(take-nth 2 bindings))
                  (do ~@body)
                  )))

;;Acts like merge when only one of the maps has a given entry, passes the decision to fn when there are
;; more than one. Considers nil to not be an entry
(defn merge-map [fn & maps]
  (let [keycoll (reduce into #{} (map keys maps))]
    (into {}
          (for [k keycoll
                :let [entries (vec (keep #(get %1 k) maps))]
                :when (not (empty? entries))]
            [k (if (= 1 (count entries))
                 (entries 0)
                 (apply fn entries))]))))


;; Head tools

(defn get-meta-elements []
  (let [head (first (seq (. js/document (getElementsByTagName "head"))))]
    (seq (. head (getElementsByTagName "meta")))))

(defn get-meta-map-by-property [& elements]
  (let [elements (or elements (get-meta-elements))]
    (into {}
          (for [e elements
                :let [prop (. e (getAttribute "property"))]
                :when prop]
            [prop e]))))

(defn add-meta-tag! [el]
  (let [head (first (seq (. js/document (getElementsByTagName "head"))))]
    (. head (appendChild el))))

(defn set-meta-property! [property content]
  (if-let [el ((get-meta-map-by-property) property)]
    (. el (setAttribute "content" content))
    (let [el (. js/document (createElement "meta"))]
      (. el (setAttribute "property" property))
      (. el (setAttribute "content" content))
      (add-meta-tag! el))))



(defn loading-indicator []
  [:div "Loading..."])
