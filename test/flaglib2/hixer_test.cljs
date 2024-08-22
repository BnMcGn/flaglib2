(ns flaglib2.hixer-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [day8.re-frame.test :as rf-test]
     [flaglib2.hixer :as hixer]
     [flaglib2.displayables :as disp]
     [re-frame.core :as rf]))

(def good1 [[:a {:href "http://there/"} "is"]
            [:div [:h1 "thing"] [:p "thing"]]
            ['disp/thread-opinion]])

(def bad1 [[:div {:class "less"}]])
(def bad2 [:a :b :c])
(def bad3 [:form])
(def bad4 [:a {:href :that}])
(def bad5 ['x])

(deftest check-hiccup
  (is (not (hixer/check-hiccup good1)))
  (is (thrown-with-msg? js/Error #"Only strings and vectors" (hixer/check-hiccup bad1)))
  (is (thrown-with-msg? js/Error #"Only strings and vectors" (hixer/check-hiccup bad2)))
  (is (thrown-with-msg? js/Error #"known element type" (hixer/check-hiccup bad3)))
  (is (thrown-with-msg? js/Error #"attrib must be a string" (hixer/check-hiccup bad4)))
  (is (thrown-with-msg? js/Error #"known display component" (hixer/check-hiccup bad5))))
