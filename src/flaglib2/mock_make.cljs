(ns flaglib2.mock-make
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]

   [flaglib2.fabricate :as fab]

   ))


{:flaglib2.posters/alternate-response nil,
 :title-store {},
 :opinion-store {},
 :flaglib2.fabricate/specify-target {:on-select nil, :suppress-search-results false},
 :flaglib2.posters/opinion-failure nil,
 :server-parameters {},
 :text-store {},
 :warstats-store {},
 :flaglib2.posters/opinion-response nil,
 :flaglib2.posters/alternate-failure nil,

 :flaglib2.fetchers/author-urls {}

 }
