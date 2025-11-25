(ns flaglib2.mock-vis
  (:require
   [cljs-time.core :as tm]
   [re-frame.alpha :as rf]

   [flaglib2.misc :as misc]
   [flaglib2.target :as targ]
   [flaglib2.things :as things]
   ))


(def plain-db
  {:title-store {},
   :opinion-store {},
   :text-store {},
   :warstats-store {},
   :local {:advanced true}})

(def rooturl "http://google.com/")

(def plain-opinion
  {:rooturl rooturl
   :authorname "Kilroy"
   :created (tm/now)})

(defn tid [num]
  (str "pnnkaeeeebaeebaeebaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebaeee"
       (.padStart (str num) 2 "0")))

(def opinions
  {(tid 0)
   (assoc plain-opinion
          :target rooturl
          :flag :negative-dislike
          :tree-address (list (tid 0))
          :clean-comment "This is a good opinion")
   (tid 1)
   (assoc plain-opinion
          :target (tid 0)
          :flag :positive-evidence
          :tree-address (list (tid 0) (tid 1))
          :clean-comment "This is a bad opinion. It should have a warn off"
          :reference rooturl)
   (tid 2)
   (assoc plain-opinion
          :target rooturl
          :flag :custodial-blank
          :tree-address (list (tid 2))
          :clean-comment "This is a boring opinion. It should be hidden.")
   (tid 3)
   (assoc plain-opinion
          :target (tid 2)
          :flag :positive-like
          :tree-address (list (tid 2) (tid 3))
          :clean-comment "This is an exciting opinion.")
   (tid 4)
   (assoc plain-opinion
          :target rooturl
          :flag :negative-needs-evidence
          :tree-address (list (tid 4))
          :clean-comment "This is a sidelined opinion")
   (tid 5)
   (assoc plain-opinion
          :target (tid 4)
          :flag :positive-agree
          :tree-address (list (tid 4) (tid 5))
          :clean-comment "This opinion is sidelined by its parent")})

(def opinion-tree
  `((~(tid 0) (~(tid 1)))
    (~(tid 2) (~(tid 3)))
    (~(tid 4) (~(tid 5)))))

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
  {(tid 0)
   (assoc basic-warstat
          :x-up 30)
   (tid 1)
   (assoc basic-warstat
          :negative-out-of-bounds 20
          :negative-disturbing 10)
   (tid 2)
   (assoc basic-warstat
          :x-down 5)
   (tid 3)
   (assoc basic-warstat
          :x-up 10)
   (tid 4)
   (assoc basic-warstat
          :custodial-offtopic 10
          :x-up 5)
   (tid 5)
   (assoc basic-warstat
          :x-up 2)})

(def vis-lister
  {:things1
   [{:id (tid 0) :type :opinion}
    {:id (tid 1) :type :opinion}
    {:id (tid 2) :type :opinion}
    {:id (tid 3) :type :opinion}
    {:id (tid 4) :type :question}
    {:id (tid 5) :type :opinion}]})

(defn mock-vis [_]
  [:div
   [targ/target-root-thread :rooturl rooturl]
   [things/thing-lister :vis-lister]])

(rf/reg-event-fx
 :mock-vis
 (fn [{:keys [db]} _]
   (let []
     {:db
      (assoc db
             :opinion-store opinions
             :opinion-tree-store
             {rooturl opinion-tree}
             :warstats-store warstats
             :thing-listers
             (assoc (:thing-listers db) :vis-lister vis-lister)
             :root-element mock-vis)
      :fx [[:dispatch [:mount-registered]]]})))
