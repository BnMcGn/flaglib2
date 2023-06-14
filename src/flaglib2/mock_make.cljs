(ns flaglib2.mock-make
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]

   [flaglib2.fabricate :as fab]
   [flaglib2.stepper :as step]
   [flaglib2.misc :as misc]

   ))


(def plain-db
  {:flaglib2.posters/alternate-response nil,
   :title-store {},
   :opinion-store {},
   :flaglib2.fabricate/specify-target {:on-select nil, :suppress-search-results false},
   :flaglib2.posters/opinion-failure nil,
   :text-store {},
   :warstats-store {},
   :flaglib2.posters/opinion-response nil,
   :flaglib2.posters/alternate-failure nil,

   :flaglib2.fetchers/author-urls {}})

(def sections
  {:initial plain-db})

(defn mock-make [{:keys [section]}]
  [step/wf-stepper])

(rf/reg-event-fx
 :mock-make
 (fn [{:keys [db]} _]
   (let [section (keyword (get-in db [:server-parameters :section]))
         db (merge
             db
             (get sections section)
             {:root-element mock-make})]
     {:db db
      :fx [
           ;;[:dispatch [:add-hooks fabricate-hooks]]
           [:dispatch [:flaglib2.stepper/initialize fab/steps]]
           [:mount-registered db]]})))



