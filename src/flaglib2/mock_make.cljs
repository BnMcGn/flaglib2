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


(def title-store
  {"http://www.columbia.edu/~fdc/sample.html"
   {:x-up 1, :x-down 1, :x-wrong 0, :x-right 0,
    :initial-status "failure",
    :controversy 4,
    :title "Sample Web Page",
    :title-source :initial,
    :replies-immediate 0,
    :tree-freshness "2021-02-01T14:53:58+0000",
    :initial-message "Failed to load URL",
    :replies-total 8,
    :hashtags nil, :effect 0,
    :direction :neutral,
    :direction-on-root :neutral}})

(def text-store
  {"http://www.columbia.edu/~fdc/sample.html"
   {:x-up 1, :x-down 1, :x-wrong 0, :x-right 0,
    :initial-status "failure",
    :controversy 4,
    :text "",
    :text-source :initial,
    :replies-immediate 0,
    :tree-freshness "2021-02-01T14:53:58+0000",
    :initial-message "Failed to load URL",
    :replies-total 8,
    :hashtags nil, :effect 0,
    :direction :neutral,
    :direction-on-root :neutral}})

(def warstats-store
  {"http://www.columbia.edu/~fdc/sample.html"
   {:x-up 1, :x-down 1, :x-wrong 0, :x-right 0,
    :controversy 4,
    :replies-immediate 0,
    :tree-freshness (misc/parse-time "2021-02-01T14:53:58+0000"),
    :replies-total 8,
    :hashtags nil, :effect 0,
    :direction :neutral,
    :direction-on-root :neutral}})

(def target-url "http://www.columbia.edu/~fdc/sample.html")

(def specify-target
  {:on-select nil,
   :suppress-search-results true,
   :flaglib2.urlgrab/search target-url,
   :flaglib2.urlgrab/selection target-url})

(def warstats-store-unreviewed
  (assoc-in warstats-store [target-url :replies-total] 0))

(def text-store-unavailable
  (update-in text-store [target-url] dissoc :text))

(def text-failed {:status "failure" :message "Failure is happened!!"})

(def targetted-db
  (merge
   plain-db
   {:title-store title-store
    :text-store text-store
    :warstats-store warstats-store
    :flaglib2.fabricate/specify-target specify-target}))

(def sections
  {:initial plain-db
   :opine targetted-db
   :decision-reviewed targetted-db
   :decision-available (assoc targetted-db :warstats-store warstats-store-unreviewed)
   :decision-wait (assoc targetted-db
                         :warstats-store warstats-store-unreviewed
                         :text-store text-store-unavailable)
   :decision-failure (assoc targetted-db
                            :warstats-store warstats-store-unreviewed
                            :text-store text-store-unavailable
                            :text-status text-failed)})

(def section-step {:opine :opine
                   :decision-reviewed :target-decision
                   :decision-available :target-decision
                   :decision-wait :target-decision
                   :decision-failure :target-decision})


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
     (when-not (get sections section) (throw (js/Error. "Mockable not found")))
     {:db db
      :fx [
           ;;[:dispatch [:add-hooks fabricate-hooks]]
           [:dispatch [:flaglib2.stepper/initialize fab/steps]]
           (when-let [step (get section-step section)]
             [:dispatch [:flaglib2.stepper/goto step]])
           [:mount-registered db]]})))



