(ns flaglib2.stepper
  (:require
   [clojure.string :as string]
   [re-frame.alpha :as rf]
   [flaglib2.misc :as misc]
   [flaglib2.deco :as deco]

   [re-com-tailwind.core :as rc]
   [re-com-tailwind.functions :refer [tw-btn-primary]]))

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
;; :next, :previous - optional id of where the Next/Previous buttons should jump when activated.
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

(defn step-style [active? grouped?]
  (let [base "border-solid border-8 rounded-lg m-2 p-2"
        color (if grouped?
                (if active? "border-stripes" "border-blue-200")
                (if active? "border-blue-200" "border-slate-200"))
        margin (when grouped? "ml-4")]
    (string/join " " [base color margin])))

(defn step-base [step buttons]
  [rc/box
   :class (step-style true (:grouped step))
   :child
   [:div
    {:style {:flex-grow "1"}}
    (:page step)
    buttons]])

(defn next-button [label]
  (let [indpos @(rf/subscribe [::index-pos])]
    [rc/button
     :class (tw-btn-primary)
     :label (if (string? label) label "Next")
     :on-click #(rf/dispatch [::next])
     :disabled? (if (< (:index indpos) (:count indpos)) false true)]))

(defn next-button-disabled []
  [rc/button
   :label "Next"
   :class deco/button-disabled
   :disabled? true])

(defn previous-button [label]
  (let [indpos @(rf/subscribe [::index-pos])
        disabled (< (:index indpos) 1)]
    [rc/button
     :label (if (string? label) label "Previous")
     :on-click #(rf/dispatch [::previous])
     :class (when disabled deco/button-disabled)
     :disabled? disabled]))

(defn button-box [contents]
  [rc/h-box
   :class "mt-2 bg-gray-200 relative px-12 py-2"
   :style {:top "0.5rem" :left "-0.5rem" :width "calc(100% + 1rem)"}
   :children contents])

(defn button-spacer [fore aft]
  (let [one (reduce #(into %1 [[rc/gap :size "3em"] %2]) (cons [] fore))
        two (reduce #(into %1 [[rc/gap :size "3em"] %2]) (cons [] aft))]
    (reduce
     into [[]
           (drop 1 one)
           [[rc/gap :style {:flex-grow "1"} :size "1em"]]
           (drop 1 two)])))

(defn stepper-buttons [& {:keys [next previous buttons]
                          :or {previous true next true}}]
  (let [next (when next
               [next-button next])
        previous (when previous
                   [previous-button previous])]
    [button-box
     (button-spacer
      (when previous [previous])
      (if buttons (into buttons [next]) [next]))]))

(defn summary-button [id label]
  [rc/button :label label
   :class "overflow-hidden max-w-full"
   :parts {:wrapper {:class "w-full"}}
   :on-click (fn [] (rf/dispatch [::goto id]))])

(defn step-display [step]
  (cond
    (= :summary (:status step))
    (when (:label step)
      [rc/box
       :class (step-style false (:grouped step))
       :child
       (if (string? (:label step))
         [summary-button (:id step) (:label step)]
         (:label step))])
    (= :active (:status step))
    [step-base step (or (:buttons step) (stepper-buttons))]))

(defn wf-stepper []
  (let [steplist @(rf/subscribe [::steplist])
        steps @(rf/subscribe [::steps])]
    (into [:div {:class "max-w-[70em]"}]
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
         steps (assoc (::steps db)
                      old-active
                      (assoc (get-in db [::steps old-active]) :status :summary)
                      target
                      step)]
     {:db (assoc db ::steps steps)
      :fx [ (when (:every step) [:dispatch (:every step)])
           (when once [:dispatch once])]})))

(rf/reg-event-fx
 ::visit
 (fn [{:keys [db]} [_ targets]]
   (let [steps (map #(get-in db [::steps %]) targets)]
     {:fx (into []
                (for [s steps
                      item [(:every s) (:once s)]
                      :when item]
                  [:dispatch item]))})))

(defn get-next-step-in-order [db]
  (let [stepid (find-active-step (::steps db))
        steplist (::steplist db)
        stepind (misc/first-index stepid steplist)]
    (and stepind
         (< stepind (count steplist))
         (nth steplist (+ 1 stepind)))))

(rf/reg-event-fx
 ::next
 (fn [{:keys [db]} _]
   (let [stepid (find-active-step (::steps db))
         next (get-in db [::steps stepid :next])
         next (if (fn? next) (next db) next)
         next (or next (get-next-step-in-order db))]
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

(defn set-step-state [{:keys [id] :as step} active summarize]
  (assoc step
         :status
         (cond (= id active) :active
               (summarize id) :summary ;;Summarize should be a set
               :else :hidden)))

(rf/reg-event-fx
 ::initialize
 (fn [{:keys [db]} [_ steps {:keys [active summarize]}]]
   (let [steplist (map :id steps)
         summarize (or summarize #{})
         active (or active (first steplist))]
     {:db (assoc
           db
           ::steps
           (into {}
                 (for [step steps]
                   [(:id step) (set-step-state step active summarize)]))
           ::steplist steplist)
      :fx [[:dispatch [::visit summarize]]
           [:dispatch [::goto active]]]})))

(rf/reg-event-db
 ::set-summary
 (fn [db [_ step]]
   (assoc-in db [::steps step :status] :summary)))



