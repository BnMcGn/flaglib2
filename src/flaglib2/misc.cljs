(ns flaglib2.misc
  (:require
   [cljs-time.format]
   [cljs-time.coerce :as timec]
   [cljs-time.core :as time]
   [reagent.dom.server :refer [render-to-string]]
   [re-frame.registrar]
   [re-frame.alpha :as rf]
   [re-frame.db]
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.walk :as walk]
   [goog.string :as gstring]
   [goog.object :as go]
   [goog.Uri :as uri]
   [camel-snake-kebab.core :as csk]))

;; Opinion url recognizer

(def iid-pattern #"pnn[a-z0-9]{56}")

(defn iid? [item]
  (when (string? item) (re-matches iid-pattern item)))

(defn normalize-iid [item]
  (or
   (iid? item)
   (let [path (.getPath (uri/parse item))]
     (when (string/starts-with? path "/o/")
       (iid? (subs path 3))))))

(defn is-string-integer? [itm]
  (and (string? itm)
       (every? #(< 47 (.charCodeAt % 0) 58) itm)))

;;We shouldn't post to some targets. Offer adjustments when possible.

(defn target-adjust-warflagger [purl]
  (let [path (.getPath purl)]
    (cond
      (string/starts-with? path "/o/")
      {:adjusted (normalize-iid path)
       :message "Adjusted to target the opinion IID"}
      (string/starts-with? path "/target/")
      (let [tid (subs path 8)]
        (if (is-string-integer? tid)
          {:message "Please target the original rooturl, not the discussion page."}
          {:message "Adjusted to target the original rooturl"
           :adjusted (js/decodeURIComponent tid)}))
      :else {})))

(defn target-adjust-archive-org [purl]
  (let [path (.getPath purl)]
    (let [pattern #"/web/\d*/(.*)"
          res (re-matches pattern path)]
      (if res {:adjusted (js/decodeURIComponent (second res))
               :message "Adjusted to target the original unarchived url"}
          {}))))

(defn target-adjust [url]
  (let [purl (uri/parse url)]
    (case (.getDomain purl)
      "warflagger.net" (target-adjust-warflagger purl)
      "warflagger.com" (target-adjust-warflagger purl)
      "web.archive.org" (target-adjust-archive-org purl)
      {})))

(defn make-opinion-url [opinion]
  (str "/o/" (:iid opinion)))

(defn make-author-url [author]
  (str "/u/" (js/encodeURIComponent author)))

(defn make-target-url [identifier]
  (str "/target/" (js/encodeURIComponent identifier)))

(defn make-social-image-url [identifier]
  (str (go/get js/window "SERVER") "/social-image/" identifier))

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

(defn is-question? [warstats]
  (:question warstats))

(defn is-answered? [warstats]
  (:question-answered warstats))

(defn is-list-of-things? [warstats]
  (let [q (is-question? warstats)]
    (when q
      (set/intersection (set q) #{:tag :replies}))))

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

(defn truncate [string len]
  (when (string? string)
    (let [slen (count string)]
      (if (< len slen)
        (str (subs string len) "…")
        string))))

(defn first-index [itm coll & {:keys [test] :or {test =}}]
  (first (keep-indexed #(when (test %2 itm) %1) coll)))

(defn part-on-index [coll index]
  (if (and (integer? index) (<= 0 index))
    (let [tail (nthrest coll index)]
      (if (empty? tail)
        [coll]
        [(take index coll) tail]))
    [coll]))

(defn part-on-true [predicate coll]
  (part-on-index coll (first (keep-indexed #(when (predicate %2) %1) coll))))

(defn class-string [& colls]
  (string/join " " (flatten colls)))

(defn entities [itm]
  (gstring/unescapeEntities itm))

(defn pluralize [countable label]
  (let [ct (if (integer? countable) countable (count countable))]
    (if (= 1 ct)
      label
      (str label "s"))))

(defn pairify [listish]
  (loop [input listish
         stor []]
    (if (empty? input)
      stor
      (recur (drop 2 input) (conj stor [(first input) (second input)])))))

(defn map-keys [func tree]
  (let [f (fn [[k v]] [(func k) v])]
    (walk/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) tree)))

(defn kebabikey [m]
  (map-keys (fn [k] (if (string? k) (csk/->kebab-case-keyword k) k)) m))

(defn decapikey [tree]
  (map-keys (fn [k] (if (string? k) (keyword (string/lower-case k)) k)) tree))

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

(defn fake-event [evt db]
  (let [ename (first evt)
        entry (re-frame.registrar/get-handler :event ename)
        interceptor (:before (last entry))]
    (:effects (interceptor {:coeffects {:event evt :db db}}))))

(defn last-target []
  (let [{:keys [server-parameters]} (reframe-db)
        {:keys [rooturl focus]} (:default server-parameters)]
    (or (last focus) rooturl)))

(defn core-db-report [key]
  (let [places [:warstats-store :text-store :title-store :opinion-store :opinion-tree-store
                :references :refd]
        db (reframe-db)]
    (filter #(get (get db %) key) places)))

(defn target-record [place & key]
  (get-in (reframe-db) [place (or key (last-target))]))

;;Re-frame tools

(defn append-dispatch [fx & dispatches]
  (let [dispatches (for [d dispatches
                         :when d]
                     [:dispatch d])]
    (update fx :fx #(into (or % []) dispatches))))

(defn prepend-dispatch [fx & dispatches]
  (let [dispatches (for [d dispatches
                         :when d]
                     [:dispatch d])]
    (update fx :fx #(into (into [] dispatches) (or % [])))))


(defn relative-to-range
  "Returns a value indicating where num is positioned relative to start and end. If num lies between start and end, the return value will be between 0.0 and 1.0."
  [start end num]
  (if (= start end)
    0
    (/ (- num start) (- end start))))

(defn as-in-range
  "Complement of relative-to-range function. Treats num as if it were a fraction of the range specified by start and end. Returns the absolute number that results."
  [start end num]
  (if (= start end)
    0
    (+ start (* num (- end start)))))

(defn vast-majority?
  "Is A at least 1000% larger than B?"
  [a b]
  (and (< 0 a)
       (< (relative-to-range 0 a b) 0.1)))

(defn significant-majority?
  "Is B 70% or less of the size of A?"
  [a b]
  (and (< 0 a)
       (< (relative-to-range 0 a b) 0.7)))

(defn significant-majority-of?
  "Does A make up a significant majority of B"
  [a b]
  (significant-majority? a (- b a)))


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


;; Hook effects

;;Designed for situations where we occasionally want an interception to happen.

(rf/reg-fx
 ::hook-trigger
 (fn [events]
   (run! rf/dispatch events)))

(def after-hook
  (rf/->interceptor
   :id :after-hook
   :after (fn [context]
            (let [event (get-in context [:coeffects :event])
                  evid (first event)
                  entries (get-in context [:effects :db ::hooks evid])
                  inject #(case %
                            ::context context
                            ::event event
                            %)]
              (if entries
                (assoc-in context [:effects ::hook-trigger]
                          (map #(into [] (map inject %)) entries))
                context)))))

;;FIXME: Might want to add multiple calls per hook
(rf/reg-event-db
 :add-after-hooks
 (fn [db [_ hookspecs]]
   (assoc db ::hooks
          (merge-with
           set/union
           (::hooks db)
           (into {}
                 (for [[k v] hookspecs]
                   [k #{v}]))))))


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

(defn loading-indicator []
  [:div "Loading..."])
