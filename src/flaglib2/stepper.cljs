(ns flaglib2.stepper
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]
   [flaglib2.misc :as misc]
   [re-com.core :as rc]))

;;;

;; Data format for stepper:

;; A vector of maps.
;; each map describes a step

;; Fields in the step:
;; :id - name of the step, probably a keyword
;; :page - hiccup description of the step, when active
;; :buttons - optional navigation buttons to be placed at bottom of step
;; :label - summary depiction of step when completed. Can be a string or hiccup.
;; :every - an event to fire whenever the step is activated
;; :once - an event that gets run on first load of the step
;; :next, :previous - optional id of where the Next/Previous buttons should jump when 
(defn find-active-step [steps]
  (first
   (for [[k settings] steps
         :when (= :active (:status settings))]
     k)))

(rf/reg-sub ::steplist :-> ::steplist)
(rf/reg-sub ::steps :-> ::steps)

(rf/reg-sub
 ::index-pos
 :<- [::steps]
 :<- [::steplist]
 (fn [[steps steplist] _]
   (let [count (count steplist)
         active (find-active-step steps)
         index (misc/first-index active steplist)]
     {:count count :active active :index index})))


(defn step-base [step]
  [rc/box
   :child
   [:div
    (:page step)
    [rc/h-box :children (:buttons step)]]])

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


(defn stepper-buttons [& {:keys [next previous buttons]
                          :or {previous true next true}}]
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

(defn summary-button [id label]
  [rc/button :label label
   :on-click (fn [] (rf/dispatch [::goto id]))])

(defn step-display [step]
  (cond
    (= :summary (:status step))
    (if (string? (:label step))
      [summary-button (:id step) (:label step)]
      (:label step))
    (= :active (:status step))
    [step-base (assoc step :buttons (or (:buttons step) (stepper-buttons)))]))

(defn wf-stepper []
  (let [steplist @(rf/subscribe [::steplist])
        steps @(rf/subscribe [::steps])]
    (into [:div]
          (for [id steplist
                :let [step (get steps id)]
                :when (and (:status step)
                           (not (= :hidden (:status step))))]
            [step-display step]))))



(rf/reg-event-fx
 ::goto
 (fn [{:keys [db]} [_ target]]
   (let [old-active (find-active-step (::steps db))
         step (get-in db [::steps target])
         once (:once step)
         step (if once (dissoc step :once) step)
         step (assoc step :status :active)
         steps (assoc [::steps db]
                      old-active
                      (assoc (get-in db [::steps old-active]) :status :summary)
                      target
                      step)]
     {:db (assoc db ::steps steps)
      :fx [ (when (:every step) [:dispatch (:every step)])
            (when once [:dispatch once])]})))

(rf/reg-event-fx
 ::next
 (fn [{:keys [db]} _]
   (let [stepid (find-active-step (::steps db))
         next (or (get-in db [::steps stepid :next])
                  (let [steplist (::steplist db)
                        stepind (misc/first-index stepid steplist)]
                    (and stepind
                         (< stepind (count steplist))
                         (get steplist (+ 1 stepind)))))]
     (when next
       {:dispatch [::goto next]}))))

(rf/reg-event-fx
 ::previous
 (fn [{:keys [db]} _]
   (let [stepid (find-active-step (::steps db))
         previous (or (get-in db [::steps stepid :previous])
                      (let [steplist (::steplist db)
                            stepind (misc/first-index stepid steplist)]
                        (and stepind
                             (< 0 stepind)
                             (get steplist (- stepind 1)))))]
     (when previous
       {:dispatch [::goto previous]}))))

(rf/reg-event-db
 ::initialize
 (fn [db [_ params]]
   (let [steplist (map :id params)
         active (first steplist)]
     (assoc
      db
      ::steps
      (into {}
            (for [step (map #(assoc % :status (if (= (:id %) active) :active :hidden)) params)]
              [(:id step) step]))
      ::steplist steplist))))

(rf/reg-event-db
 ::set-summary
 (fn [db [_ step]]
   (assoc-in db [::steps step :status] :summary)))



