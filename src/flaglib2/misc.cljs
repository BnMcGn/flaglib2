(ns flaglib2.misc
  (:require
   [cljs-time.format]
   [cljs-time.coerce :as timec]
   [cljs-time.core :as time]
   [reagent.dom.server :refer [render-to-string]]
   [re-frame.registrar]
   [re-frame.core :as rf]
   [re-frame.db]
   [clojure.string :as string]))

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
  (re-matches url-pattern item))

(defn url-domain [url]
  (let [domain (-> url
                   (string/split "//")
                   second
                   (string/split "/")
                   first)]
    (if (string/starts-with? domain "www.")
      (subs domain 4)
      domain)))

(defn first-index [itm coll & {:keys [test] :or {test =}}]
  (first (keep-indexed #(when (test %2 itm) %1) coll)))

(defn class-string [& colls]
  (string/join " " (flatten colls)))

;;Debugging tools

(defn say [itm]
  (do (println itm)
      itm))

(defn say-when [cond itm]
  (do (when cond (println itm))
      itm))

(defn say-hiccup [itm]
  (println "SOURCE:")
  (println itm)
  (println "HTML:")
  (println (render-to-string itm))
  itm)

(defn dump [itm]
  (do (set! (. js/window -dumped) itm)
      itm))

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

(defn reframe-db []
  "Fetch the reframe db. Used for testing/console. Not for live code!"
  @re-frame.db/app-db)





(defn relative-to-range [start end num]
  "Returns a value indicating where num is positioned relative to start and end. If num lies between start and end, the return value will be between 0.0 and 1.0."
  (/ (- num start) (- end start)))

(defn as-in-range [start end num]
  "Complement of relative-to-range function. Treats num as if it were a fraction of the range specified by start and end. Returns the absolute number that results."
  (+ start (* num (- end start))))


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
        [k urls] (or l {})
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
