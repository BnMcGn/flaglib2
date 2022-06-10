(ns flaglib2.suggester
  (:require
    [re-com.box        :refer [h-box v-box box gap line flex-child-style align-style]] ;; need?
    
    [flaglib2.misc :as misc]
    [re-frame.core :as rf]
    [reagent.core      :as    reagent]
    [goog.events.KeyCodes]))

(defn- activate-suggestion-by-index
  "Make the suggestion at `index` the active suggestion"
  [{:as state :keys [suggestions]} index]
  (let [suggestion (nth suggestions index)]
    (assoc state :suggestion-active-index index)))

(defn- wrap [index count] (mod (+ count index) count))

(defn- activate-suggestion-next
  [{:as state :keys [suggestions suggestion-active-index]}]
  (cond-> state
    (seq suggestions)
    (activate-suggestion-by-index (-> suggestion-active-index (or -1) inc (wrap (count suggestions))))))

(defn- activate-suggestion-prev
  [{:as state :keys [suggestions suggestion-active-index]}]
  (cond-> state
    (seq suggestions)
    (activate-suggestion-by-index (-> suggestion-active-index (or 0) dec (wrap (count suggestions))))))

(rf/reg-sub
 ::suggester
 (fn [db [_ location]]
   (get-in db location)))

(defn suggester-keydown-handler! [location event]
  (condp = (.-which event)
    goog.events.KeyCodes.UP (rf/dispatch [::activate-suggestion-prev location])
    goog.events.KeyCodes.DOWN (rf/dispatch [::activate-suggestion-next location])
    goog.events.KeyCodes.ENTER (rf/dispatch [::choose-suggestion-active location])
    goog.events.KeyCodes.ESC (rf/dispatch [::activate-suggestion-prev location])
    goog.events.KeyCodes.TAB
    (let [suggester @(rf/subscribe [::suggester location])]
      (if (not-empty (:suggestions suggester))
        (do (.preventDefault event)
            (rf/dispatch [::activate-suggestion-next location]))
        (rf/dispatch [::hide-suggester location])))
    true))

(rf/reg-event-db
 ::activate-suggestion-next
 (fn [db [_ location]]
   (update-in db location activate-suggestion-next)))

(rf/reg-event-db
 ::activate-suggestion-prev
 (fn [db [_ location]]
   (update-in db location activate-suggestion-prev)))

(rf/reg-event-db
 ::activate-suggestion-by-index
 (fn [db [_ location index]]
   (update-in db location #(assoc %1 :suggestion-active-index index))))

(rf/reg-fx
 ::call-on-select
 (fn [[func selection]]
   (when func
     (func selection))))

(rf/reg-event-fx
 ::on-select
 (fn [{:keys [db]} [_ location & selection]]
   (let [state (get-in db location)]
     {::call-on-select [(:on-select state)
                        (or selection (get (:suggestions state) (:suggestion-active-index state)))]})))

(rf/reg-event-fx
 ::select
 (fn [{:keys [db]} [_ location & index]]
   (if index
     {:dispatch [::on-select location index]}
     (let [index (get (get-in db location) :suggestion-active-index)]
       {:db (update-in db location #(assoc %1 :suggestion-active-index index))
        :fx  [[:dispatch [::on-select location index]]]}))))

(rf/reg-event-db
 ::initialize-suggester
 (fn [db [_ location state]]
   (let [loc (get-in db location)]
     (if loc
       ;;For now, we only initialize if location is empty. Can add an option in 'state' to change that
       db
       (assoc-in db location state)))))

(rf/reg-event-db
 ::delete-suggester
 (fn [db [_ location]]
   (update-in db (butlast location) dissoc (last location))))


(defn suggester-component
  [location init]
  (let [{:as state
         :keys [suggestions suggestion-active-index input render-suggestion]}
        @(rf/subscribe [::suggester location])]
    [box
     :style {:position "relative"}
     :child
     [v-box
      ;;We omit the waiting? code
      :children
      [(for [[i s] (map vector (range) suggestions)
             :let [selected? (= suggestion-active-index i)]]
         ^{:key i}
         [box
          :child (if render-suggestion
                   (render-suggestion s)
                   s)
          :class (str "rc-typeahead-suggestion" (when selected? " active"))
          :attr {:on-mouse-over #(rf/dispatch [::activate-suggestion-by-index location i])
                 :on-mouse-down #(do (.preventDefault %)
                                     (rf/dispatch [::select location i]))}])]]]))

(defn suggester
  [& {:as state :keys [location]}]
  (let [loc location]
    (rf/dispatch-sync [::initialize-suggester loc state])
    (fn [state]
      (suggester-component loc state))))

;;FIXME: is with-let broken?
(defn suggesterx
  [& {:as state :keys [location]}]
  (reagent/with-let
    [loc (or location [::suggesters (keyword *ns* (gensym "suggester"))])]
    (rf/dispatch-sync [::initialize-suggester loc state])
    (suggester-component loc state)
    'reagent/finally
    (when-not location
      (rf/dispatch [::delete-suggester loc]))))


