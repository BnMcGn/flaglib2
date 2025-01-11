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


(def positive-warstat
  {:x-up 3 :x-down 0 :x-right 0 :x-wrong 0})
(def db-1 {:refd {"asdf" [:one]}})

(deftest in-a-word
  (is (= :contested (mood/in-a-word contested-warstat-1)))
  (is (= :unsupported
         (mood/in-a-word {:x-up 10 :x-down 0 :x-right 0 :x-wrong 8})))
  (is (= :awkward
         (mood/in-a-word {:x-up 0 :x-down 10 :x-right 10 :x-wrong 0})))
  (is (= :ignored
         (mood/in-a-word {:x-up 0 :x-down 0 :x-right 0 :x-wrong 0})))
  (is (= :restricted
         (mood/in-a-word {:x-up 10 :x-down 0 :x-right 0 :x-wrong 0 :negative-spam 8})))
  (is (= :sidelined
         (mood/in-a-word {:x-up 10 :x-down 0 :x-right 0 :x-wrong 0 :custodial-arcane 8})))
  (is (= :contested
         (mood/in-a-word {:x-up 10 :x-down 6 :x-right 0 :x-wrong 0 :custodial-arcane 6})))
  (is (= :positive (mood/in-a-word positive-warstat :key "qwert" :db db-1)))
  (is (= :significant (mood/in-a-word positive-warstat :key "asdf" :db db-1))))

