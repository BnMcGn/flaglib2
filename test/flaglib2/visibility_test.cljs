(ns flaglib2.visibility-test
  (:require
   [cljs.test :refer-macros [deftest is testing async]]
   [day8.re-frame.test :as rf-test]
   [cljs-time.core :as tm]

   [clojure.string :as string]
   [goog.string :as gstring]
   [goog.string.format]

   [re-frame.alpha :as rf]
                                        ;[re-frame.flow.alpha :as flow]

   [flaglib2.visibility :as vis]
   [flaglib2.misc :as misc]
   ))

(defn tid [num]
  (str "pnnkaeeeebaeebaeebaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebaeee"
       (.padStart (str num) 2 "0")))


(def rooturl "http://fake.fake/")

(def created "2020-03-16T06:29:59+0000")


(def plain-opinion
  {:rooturl rooturl
   :target rooturl
   :authorname "Kilroy"
   :created (tm/now)})

(def opinions
  {(tid 0)
   (assoc plain-opinion
          :iid (tid 0)
          :flag :negative-dislike
          :tree-address (list (tid 0)))
   (tid 1)
   (assoc plain-opinion
          :iid (tid 1)
          :flag :negative-disagree
          :tree-address (list (tid 1)))
   (tid 2)
   (assoc plain-opinion
          :iid (tid 2)
          :flag :negative-disagree
          :tree-address (list (tid 2)))
   (tid 3)
   (assoc plain-opinion
          :iid (tid 3)
          :flag :negative-needs-evidence
          :tree-address (list (tid 3)))
   (tid 4)
   (assoc plain-opinion
          :iid (tid 4)
          :flag :negative-dislike
          :tree-address (list (tid 4)))
   (tid 5)
   (assoc plain-opinion
          :iid (tid 5)
          :flag :negative-dislike
          :tree-address (list (tid 5)))})

(def opinion-tree
  `((~(tid 0))
    (~(tid 1))
    (~(tid 2))
    (~(tid 3))
    (~(tid 4))
    (~(tid 5))))

(def basic-warstat
  {:replies-immediate 0
   :replies-total 0
   :x-right 0
   :x-wrong 0
   :x-up 0
   :x-down 0
   :effect 1
   :controversy 0
   :hashtags nil
   :x-right-source nil
   :x-up-source nil
   :x-wrong-source nil
   :x-down-source nil
   :direction :neutral
   :direction-on-root :neutral})

(def warstats
  {rooturl
   (assoc basic-warstat
          :controversy 2
          :effect 0
          :x-right 2
          :x-wrong 1
          :x-down 1)})

(def plain-db
  {:warstats-store warstats
   :opinion-store opinions
   :opinion-tree-store {rooturl opinion-tree}
   :local {}})

(deftest wrecked-visible
  (let [vstat (get (vis/visibility-func plain-db) rooturl)]
    (is (= :show (:list-display vstat)))
    (is (not (:warn-off-excerpt-only vstat)))
    (is (= :contested (:word vstat)))))

