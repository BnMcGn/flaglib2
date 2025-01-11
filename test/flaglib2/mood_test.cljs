(ns flaglib2.mood-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]

   [flaglib2.misc :as misc]
   [flaglib2.mood :as mood]))


(def contested-warstat-1
  {:x-up 1, :controversy 2, :x-down 0, :x-wrong 2, :effect 0, :x-right 1})


(deftest flavor-from-own-warstats
  (is (= :contested (mood/flavor-from-own-warstats contested-warstat-1)))
  (is (= :positive
         (mood/flavor-from-own-warstats (assoc contested-warstat-1
                                               :positive-interesting 10))))
  (is (= :negative
         (mood/flavor-from-own-warstats (assoc contested-warstat-1
                                               :custodial-offtopic 10)))))

