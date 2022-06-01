(ns flaglib2.typeahead
  (:require-macros
    [re-com.core            :refer [handler-fn at reflect-current-component]]
    [cljs.core.async.macros :refer [alt! go-loop]])
  (:require
    [cljs.core.async   :refer [chan timeout <! put!]]
    [re-com.config     :refer [include-args-desc?]]
    [re-com.debug      :refer [->attr]]
    [re-com.throbber   :refer [throbber]]
    [re-com.input-text :refer [input-text]]
    [re-com.util       :refer [deref-or-value px]]
    [re-com.popover    :refer [popover-tooltip]] ;; need?
    [re-com.box        :refer [h-box v-box box gap line flex-child-style align-style]] ;; need?
    [re-com.validate   :refer [input-status-type? input-status-types-list regex? string-or-hiccup? css-style? html-attr? parts? number-or-string?
                               string-or-atom? throbber-size? throbber-sizes-list]]

    [flaglib2.misc :as misc]
    [re-frame.core :as rf]
    [reagent.core      :as    reagent]
    [goog.events.KeyCodes]))


(defn suggester-keydown-handler! [location event]
  (condp = (.-which event)
    goog.events.KeyCodes.UP (rf/dispatch [::activate-suggestion-prev location])
    goog.events.KeyCodes.DOWN (rf/dispatch [::activate-suggestion-next location])
    goog.events.KeyCodes.ENTER (rf/dispatch [::choose-suggestion-active location])
    goog.events.KeyCodes.ESC (rf/dispatch [::activate-suggestion-prev location])
    goog.events.KeyCodes.TAB
    (let [suggestions @(rf/subscribe [::suggestions location])]
      (if (not-empty suggestions)
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

(rf/reg-event-db
 ::initialize-suggester
 (fn [db [_ location state]]
   (let [loc (get-in db location)]
     (if loc
       ;;For now, we only initialize if location is empty. Can add an option in 'state' to change that
       db
       (assoc-in db location state)))))

(defn suggester
  [{:as state :keys [location]}]
  (reagent/with-let
    [loc (or location [::suggesters (keyword *ns* (gensym "suggester"))])
     _ (rf/dispatch-sync [::initialize-suggester loc state])]
    ))
(defn suggester
  "Utility component for adding drop down suggestions to an entry."
  [& {:keys [location] :as args}]
  (let [
        {:as state :keys [c-search c-input]} (make-typeahead-state args)
        state-atom (reagent/atom state)
        input-text-model (reagent/cursor state-atom [:input-text])]
    (search-data-source-loop! state-atom c-search)
    (fn typeahead-render
      [& {:as   args
          :keys [data-source _on-change _change-on-blur? _immediate-model-update? model _debounce-delay render-suggestion _suggestion-to-string _rigid?
    ]}]
      (let [{:as state :keys [suggestions suggestion-active-index input]} @(rf/subscribe location)





            {:as state :keys [suggestions waiting? suggestion-active-index external-model]} @state-atom
            width                 (or width "250px")]
        [v-box
         :src      src
         :class    "rc-typeahead"
         :width    width
         :children
         [
          (if (or (not-empty suggestions) waiting?)
            [box
             :src   (at)
             :style {:position "relative"}
             :child [v-box
                     :src      (at)
                     :children [(when waiting?
                                  [box
                                   :src   (at)
                                   :align :center
                                   :child [throbber
                                           :src   (at)
                                           :size  :small
                                           :class (str "rc-typeahead-throbber " (get-in parts [:throbber :class]))]])
                                          (for [[i s] (map vector (range) suggestions)
                                                :let [selected? (= suggestion-active-index i)]]
                                            ^{:key i}
                                            [box
                                             :src   (at)
                                             :child (if render-suggestion
                                                      (render-suggestion s)
                                                      s)
                                             :class (str "rc-typeahead-suggestion"
                                                         (when selected? " active")
                                                         (get-in parts [:suggestion :class]))
                                             :attr {:on-mouse-over #(swap! state-atom activate-suggestion-by-index i)
                                                    :on-mouse-down #(do (.preventDefault %) (swap! state-atom choose-suggestion-by-index i))}])]]])]]))))
