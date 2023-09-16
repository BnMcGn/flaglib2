(ns flaglib2.target-summary
  (:require
   [re-frame.core :as rf]
   [reagent.core :as r]

   [re-com-tailwind.core :as rc]

   [flaglib2.misc :as misc]
   [flaglib2.ipfs]
   [flaglib2.deco :as deco]
   [flaglib2.flags :as flags]
   [flaglib2.displayables :as disp]))

(defn translate [x y]
  (str "translate(" x "," y ")"))

(def bar-width 20)
(def bar-height-max 150)
(def m [0 0 0 0])
(def centerline (+ bar-height-max 30))

(defn positive-bar [& {:keys [style label transform height title]}]
  (let [top (- centerline height)]
    [:g
     {:transform transform
      :width bar-width}
     [:rect
      {:height height
       :width bar-width
       :x 0
       :y top
       :style style}
      [:title title]]
     [:text
      {:x (* 0.5 bar-width)
       :y (- top 10)
       :text-anchor "middle"}
      label]]))

(defn negative-bar [& {:keys [style label transform height title]}]
  [:g
   {:transform transform}
   [:rect
    {:height height
     :width bar-width
     :x 0
     :y 0
     :style style}
    [:title title]]
   [:text
    {:x (* 0.5 bar-width)
     :y (+ height 23)
     :text-anchor "middle"}
    label]])

(defn summary-scores-chart [rooturl]
  (let [w (- 110 (m 1) (m 3))
        h (- (* 2 centerline) (m 0) (m 2))
        warstats @(rf/subscribe [:warstats-store rooturl])
        colsize 20
        col2 70
        highest (max (:x-right warstats) (:x-wrong warstats) (:x-up warstats) (:x-down warstats))]
    [:<>
     [:h3 "Score"]
     [:div
      {:class "border-[3px] border-black"}
      [:svg {:width (+ w (m 1) (m 3))
             :height (+ h (m 0) (m 2))
             :transform (translate (m 3) (m 0))}
       [positive-bar
        :style {:fill "#80ff80" :stroke "black" :strokeWidth "1"}
        :label (:x-up warstats)
        :title "Up"
        :height (misc/as-in-range
                 0 bar-height-max
                 (misc/relative-to-range 0 highest (:x-up warstats)))
        :transform (translate colsize 0)]
       [positive-bar
        :style {:fill "#80ff80" :stroke "black" :strokeWidth "1"}
        :label (:x-right warstats)
        :title "Right"
        :height (misc/as-in-range
                 0 bar-height-max
                 (misc/relative-to-range 0 highest (:x-right warstats)))
        :transform (translate col2 0)]
       [negative-bar
        :style {:fill "#ff8080" :stroke "black" :strokeWidth "1"}
        :label (:x-down warstats)
        :title "Down"
        :height (misc/as-in-range
                 0 bar-height-max
                 (misc/relative-to-range 0 highest (:x-down warstats)))
        :transform (translate colsize centerline)]
       [negative-bar
        :style {:fill "#ff8080" :stroke "black" :strokeWidth "1"}
        :label (:x-wrong warstats)
        :title "Wrong"
        :height (misc/as-in-range
                 0 bar-height-max
                 (misc/relative-to-range 0 highest (:x-wrong warstats)))
        :transform (translate col2 centerline)]]]]))

(defn display-other-flags [targetid]
  (let [warstats @(rf/subscribe [:warstats-store targetid])
        flags
        [:negative-inflammatory :negative-language-warning :negative-disturbing
         :negative-logical-fallacy :negative-out-of-bounds :positive-funny
         :positive-interesting :custodial-redundant :custodial-out-of-date
         :custodial-flag-abuse :custodial-offtopic :custodial-arcane]]
    (into [:div [:h3 "Flags"]]
          (for [flag flags
                :let [flinfo (flags/flags flag)]]
            [:div
             {:class "border-[3px]"
              :style (cond
                       (not (warstats flag)) {:background-color "lightgrey"
                                              :border-color "grey"}
                       (< 5 (warstats flag)) {:background-color (str (flinfo :color) "99")
                                              :border-color (flinfo :color)}
                       :else {:background-color "white"
                              :border-color (str (flinfo :color) "bb")})}
             (:label flinfo)]))))

(defn target-summary [& {:keys [rooturl]}]
  [:div
   [summary-scores-chart rooturl]
   [display-other-flags]])
