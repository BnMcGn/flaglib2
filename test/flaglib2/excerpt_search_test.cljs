(ns flaglib2.excerpt-search-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [day8.re-frame.test :as rf-test]
     [flaglib2.excerpt-search :as es]
     [flaglib2.excerpts :as excerpt]
     [re-frame.core :as rf]
     ))


(def text "Do you see it there? That _ in the event destructuring!!! Almost mocking us with that passive aggressive, understated thing it has going on!! Co-workers have said I'm \"being overly sensitive\", perhaps even pixel-ist, but you can see it too, right? Of course you can.")

(deftest basic-excerpt-search
  (rf-test/run-test-sync
   (println "in basic-excerpt-search test")
   (let [result (atom nil)
         tdat (excerpt/create-textdata text)
         ;;FIXME: uses internal knowledge. Probably shouldn't.
         location [:flaglib2.excerpt-search/excerpt-suggester]]
     ((es/excerpt-search
       :text text
       :on-change (fn [res] (reset! result res)))
      :text text)
     (println @re-frame.db/app-db)
     (rf/dispatch [:flaglib2.excerpt-search/do-search "see it" tdat])
     (println (rf/subscribe [:flaglib2.suggester/suggester location]))
     (rf/dispatch [:flaglib2.suggester/select location 0])
     (println (rf/subscribe [:flaglib2.excerpt-search/excerpt-start]))

     (rf/dispatch [:flaglib2.excerpt-search/do-search "see ityou can" tdat])
     (rf/dispatch [:flaglib2.suggester/select location 0])
     (println @result)
     (is (not @result))
     )))
