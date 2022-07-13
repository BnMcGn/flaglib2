(ns flaglib2.excerpt-search-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [day8.re-frame.test :as rf-test]
     [flaglib2.excerpt-search :as es]
     [flaglib2.excerpts :as excerpt]

     [reagent.dom :as rdom]
     [re-frame.core :as rf]
     ))


(def text "Do you see it there? That _ in the event destructuring!!! Almost mocking us with that passive aggressive, understated thing it has going on!! Co-workers have said I'm \"being overly sensitive\", perhaps even pixel-ist, but you can see it too, right? Of course you can.")

(deftest basic-excerpt-search
  (rf-test/run-test-sync
   (let [result (atom nil)
         tdat (excerpt/create-textdata text)
         el (js/document.createElement "div")
         ;;FIXME: uses internal knowledge. Probably shouldn't.
         location [:flaglib2.excerpt-search/excerpt-suggester]]
     (rdom/render
      [es/excerpt-search
       :text text
       :on-change (fn [res] (reset! result res))]
      el)
     #_((es/excerpt-search
       :text text)
      :text text
      :on-change (fn [res] (reset! result res)))

     (rf/dispatch [:flaglib2.excerpt-search/do-search "see it" tdat])
     (let [state @(rf/subscribe [:flaglib2.suggester/suggester location])
           suggests (:suggestions state)]
       (is (= 2 (count suggests)))
       (when (= 2 (count suggests))
         (rf/dispatch [:flaglib2.suggester/select location 0])))
     (let [start @(rf/subscribe [:flaglib2.excerpt-search/excerpt-start])]
       (is (= 0 (:remaining start)))
       (is (= 7 (:start-index start)))
       (is (= 12 (:end-index start))))

     (rf/dispatch [:flaglib2.excerpt-search/do-search "see ityou can" tdat])
     (let [state @(rf/subscribe [:flaglib2.suggester/suggester location])
           suggests (:suggestions state)]
       (is (= 2 (count suggests)))
       (when (= 2 (count suggests))
         (rf/dispatch [:flaglib2.suggester/select location 0])))

     (let [[excerpt offset] @result]
       (is (string? excerpt))
       (is (= 0 offset))
       (is (= 220 (count excerpt))))
     )))
