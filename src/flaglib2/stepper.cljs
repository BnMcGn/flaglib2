(ns flaglib2.stepper
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [re-com.core :as rc]))



(defn find-active-step [steps]
  (first
   (for [[k settings] steps
         :when (= :active (:status settings))]
     k)))

(defn active-step-index [steps steplist]
  (first (keep-indexed #(when (= %2 (find-active-step steps) %1)) steplist)))

(rf/reg-sub ::steplist :-> ::steplist)
(rf/reg-sub ::steps :-> ::steps)

(rf/reg-sub
 ::index-pos
 :<- [::steps]
 :<- [::steplist]
 (fn [[steps steplist] _]
   (let [count (count steplist)
         active (find-active-step steps)
         index (active-step-index steps steplist)]
     {:count count :active active :index index})))


(defn step-base [step]
  (rc/box
   :child
   [:div
    (:page step)
    [rc/h-box :children (:buttons step)]]))

(defn next-button [label]
  (let [indpos @(rf/subscribe [::index-pos])]
    [rc/button
     :label (if (string? label) label "Next")
     :on-click #(rf/dispatch [::next])
     :disabled? (if (< (:index indpos) (:count indpos)) false true)]))

(defn previous-button [label]
  (let [indpos @(rf/subscribe [::index-pos])]
    [rc/button
     :label (if (string? label) label "Previous")
     :on-click #(rf/dispatch [::previous])
     :disabled? (if (< (:index indpos) 1) true false)]))

(defn stepper-buttons [& {:keys [next previous buttons]}]
  (let [next (when next
               [next-button next])
        previous (when previous
                   [previous-button previous])]
    (reduce into
            [[]
             (when previous
               [previous [rc/gap :size "3em"]])
             buttons
             (when next
               (if buttons
                 [[rc/gap :size "3em"] next]
                 [next]))])))

(defn step-display [step]
  (cond
    (= :summary (:status step))
    (if (string? (:label step))
      [rc/button :label (:label step)
       :on-click (fn [] (rf/dispatch [::goto (:id step)]))]
      (:label step))
    (= :active (:status step))
    [step-base (assoc step :buttons (or (:buttons step) (stepper-buttons)))]))

(defn wf-stepper []
  (let [steplist @(rf/subscribe [::steplist])
        steps @(rf/subscribe [::steps])]
    (into []
          (for [id steplist
                step (get id steps)
                :when (or (not (:status step))
                          (not (= :hidden (:status step))))]
            [step-display step]))))


(rf/reg-event-db
 ::goto
 (fn [db [_ target]]
   (let [old-active (find-active-step (::steps db))]
     (assoc-in
      (assoc-in db [::steps old-active :status] :summary)
      [::steps target :status] :active))))

(rf/reg-event-fx
 ::next
 (fn [{:keys [db]} _]
   (let [active (active-step-index (::steps db) (::steplist db))]
     (when (and active (< active (count (::steplist db))))
       {:dispatch [::goto (get (::steplist db) (+ 1 active))]}))))

(rf/reg-event-fx
 ::previous
 (fn [{:keys [db]} _]
   (let [active (active-step-index (::steps db) (::steplist db))]
     (when (and active (< 0 active))
       {:dispatch [::goto (get (::steplist db) (- 1 active))]}))))

(rf/reg-event-db
 ::initialize
 (fn [db params]
   (let [steplist (map :id params)
         active (first steplist)]
     (assoc
      db
      ::steps
      (map #(assoc % :status (if (= (:id %) active) :active :hidden)) params)
      ::steplist steplist))))




