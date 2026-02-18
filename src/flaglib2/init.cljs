(ns ^:export flaglib2.init
  (:require
   [goog.dom :as gdom]
   [goog.object :as go]
   [reagent.dom :as rdom]
   [re-frame.alpha :as rf]
   [clojure.reader]

   [re-com.popover]

   [flaglib2.subscriptions]
   [flaglib2.userfig :as userfig]

   [day8.re-frame-10x :as tenx]))

(rf/reg-event-fx
 ::store-server-parameters
 (fn [{:keys [db]} [_ key params]]
   {:db (assoc-in db [:server-parameters (or key :default)] params)}))


(rf/reg-fx
 :do-mount
 (fn [{:keys [mount-point element key]}]
   (when-let [mp (gdom/getElement mount-point)]
     (rdom/render [element key] mp))))

(rf/reg-event-fx
 :mount-registered
 (fn [{:keys [db]} [_ key]]
   (let [sp (:server-parameters db)
         key (or key :default)
         spec (and sp (get sp key))
         mp (:mount-point spec)
         elt (or (:side-element spec) (:root-element db))]
     (if (and mp elt)
       {:do-mount {:mount-point mp :element elt :key key}}
       {}))))

(rf/reg-event-fx
 :remount-registered
 (fn [{:keys [db]} [_ key]]
   (if key
     {:fx [ [:dispatch [:mount-registered key]] ]}
     {:fx (into []
                (for [k (keys (:server-parameters db))]
                  [:dispatch [:mount-registered {:db db :key (or k :default)}]]))})))

(defn ^:export server-side-setup [key config]
  (let [config (js->clj config :keywordize-keys true)]
    (rf/dispatch [::store-server-parameters key config])
    (rf/dispatch [:userfig/store-user-info (js->clj (go/get js/window "USERFIGDATA"))])
    (rf/dispatch [(keyword (:entry-point config)) key])))

(def local-store-keys [:advanced :warn-off-overrides])

(defn do-save-to-local [storg]
  (doseq [[k v] storg]
    (.setItem js/localStorage (str k) v)))

(defn fetch-local-store []
  (let [res (into {}
                  (for [key local-store-keys]
                    [key (.getItem js/localStorage (str key))]))]
    (update res :warn-off-overrides clojure.reader/read-string)))

(rf/reg-cofx
 :fetch-local-store
 (fn [cofx _]
   (assoc cofx :local-store (fetch-local-store))))

(rf/reg-event-fx
 :initialize-local-store
 [(rf/inject-cofx :fetch-local-store)]
 (fn [{:keys [db fetch-local-store]}]
   {:db (assoc db :local (or fetch-local-store {}))}))

(def save-to-local [(rf/path :local) (rf/after do-save-to-local)])

(defn start-10x []
  (tenx/patch!)
  (rf/dispatch-sync [:day8.re-frame-10x.events/init tenx/project-config])
  ;(rf/clear-subscription-cache!)
  (def sroot (tenx/create-shadow-root nil))
  (rdom/render (tenx/create-style-container sroot) sroot))

;;FIXME: Beware the monkeypatch!
;; Original code doesn't know what to do with spans, especially when split across multiple
;; lines. Placement also incorrect for some single line spans
;;FIXME: Arrow only attaches to end of span, not top!
(defn calculate-optimal-position
  [node]
  (let [w (.-innerWidth   js/window) ;; Width/height of the browser window viewport including, if rendered, the vertical scrollbar
        h (.-innerHeight  js/window)
        brect-main (.getBoundingClientRect node)
        y (/ (+ (.-bottom brect-main) (.-top brect-main)) 2)
        below? (< y (quot h 2))
        brects (vec (.getClientRects node))
        multispan? (< 1 (count brects))
        brect-horiz (cond (not multispan?) brect-main
                          below? (last brects)
                          :else (first brects))
        x (if below? (.-right brect-horiz) (.-left brect-horiz))
        h-threshold-left  (quot w 3)
        h-threshold-cent  (* 2 h-threshold-left)
        [s1 s2]          ["right" "left"]
        h-position        (cond
                            (< x h-threshold-left) s1
                            (< x h-threshold-cent) "center"
                            :else s2)
        v-position        (if below? "below" "above")]
    (keyword (str v-position \- h-position))))

;;We don't do anything here, because calculate-optimal-position needs the node.
(defn calc-element-midpoint [node] node)

(set! re-com.popover/calculate-optimal-position calculate-optimal-position)
(set! re-com.popover/calc-element-midpoint calc-element-midpoint)
