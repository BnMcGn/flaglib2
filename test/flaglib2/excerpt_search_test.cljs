(ns flaglib2.excerpt-search-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [day8.re-frame.test :as rf-test]
     [flaglib2.excerpt-search :as es]
     [flaglib2.excerpts :as excerpt]
     [flaglib2.suggester :as suggest]
     [flaglib2.fabricate]
     [goog.events.KeyCodes]

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

     (rf/dispatch [:flaglib2.excerpt-search/do-search "see it" tdat])
     (let [state @(rf/subscribe [:flaglib2.suggester/suggester location])
           suggests (:suggestions state)
           status @(rf/subscribe [:flaglib2.excerpt-search/excerpt-search-status])]
       (is (= :unstarted status))
       (is (= 2 (count suggests)))
       (when (= 2 (count suggests))
         (rf/dispatch [:flaglib2.suggester/select location 0])))
     (let [start @(rf/subscribe [:flaglib2.excerpt-search/excerpt-start])
           status @(rf/subscribe [:flaglib2.excerpt-search/excerpt-search-status])]
       (is (= :complete status))
       (is (= 0 (:remaining start)))
       (is (= 7 (:start-index start)))
       (is (= 12 (:end-index start))))

     ;;Search for/select an end for the excerpt...
     (rf/dispatch [:flaglib2.excerpt-search/do-search "see ityou can" tdat])
     (let [state @(rf/subscribe [:flaglib2.suggester/suggester location])
           suggests (:suggestions state)
           status @(rf/subscribe [:flaglib2.excerpt-search/excerpt-search-status])]
       (is (= :started status))
       (is (= 2 (count suggests)))
       (when (= 2 (count suggests))
         (rf/dispatch [:flaglib2.suggester/select location 0])))

     (let [[excerpt offset] @result
           status @(rf/subscribe [:flaglib2.excerpt-search/excerpt-search-status])
           [ex2 off2] @(rf/subscribe [:flaglib2.excerpt-search/active-excerpt])]
       (is (= :complete status))
       (is (string? excerpt))
       (is (= 0 offset))
       (is (= 221 (count excerpt)))
       (is (= 0 off2))
       (is (= "s" (first ex2)))
       (is (= "n" (last ex2)))))))

(defn fake-key-event [keycode]
  (let [res js/Object.]
    (set! (.-which res) keycode)
    res))

(def keycodes goog.events.KeyCodes)

(deftest keyboard-select-start
  (rf-test/run-test-sync
   (let [tdat (excerpt/create-textdata text)
         el (js/document.createElement "div")
         location [:flaglib2.excerpt-search/excerpt-suggester]]
     (rdom/render
      [es/excerpt-search
       :text text]
      el)

     (rf/dispatch [:flaglib2.excerpt-search/do-search "see it" tdat])
     (suggest/suggester-keydown-handler! location (fake-key-event keycodes.DOWN))
     (suggest/suggester-keydown-handler! location (fake-key-event keycodes.DOWN))
     (suggest/suggester-keydown-handler! location (fake-key-event keycodes.ENTER))

     (let [start @(rf/subscribe [:flaglib2.excerpt-search/excerpt-start])]
       (is (= 0 (:remaining start)))
       (is (= 229 (:start-index start)))
       (is (= 234 (:end-index start)))))))

(deftest set-excerpt-offset
  (rf-test/run-test-sync
   (let [tdat (excerpt/create-textdata text)
         el (js/document.createElement "div")
         location [:flaglib2.excerpt-search/excerpt-suggester]]
     (rdom/render
      [es/excerpt-search
       :text text
       :excerpt "see it"
       :offset 1]
      el)

     (let [start @(rf/subscribe [:flaglib2.excerpt-search/excerpt-start])]
       (is (= 0 (:remaining start)))
       (is (= 229 (:start-index start)))
       (is (= 234 (:end-index start)))))))


(deftest missing-excerpt
  (rf-test/run-test-sync
         (let [tdat (excerpt/create-textdata text)
               el (js/document.createElement "div")
               location [:flaglib2.excerpt-search/excerpt-suggester]]
           (rdom/render
            [es/excerpt-search
             :text text]
            el)

           (rf/dispatch [:flaglib2.excerpt-search/do-search "quick brown fox" tdat])
           (suggest/suggester-keydown-handler! location (fake-key-event keycodes.ENTER))

           (let [status @(rf/subscribe [:flaglib2.excerpt-search/excerpt-search-status])]
             (is (= :failed status))))))

(deftest re-enter-excerpt
  (rf-test/run-test-sync
   (let [result (atom nil)
         tdat (excerpt/create-textdata text)
         el (js/document.createElement "div")
         location [:flaglib2.excerpt-search/excerpt-suggester]]
     (rdom/render
      [es/excerpt-search
       :text text
       :on-change (fn [res] (reset! result res))]
      el)

     ;;Set up initial
     (rf/dispatch [:flaglib2.excerpt-search/do-search "Co-workers" tdat])
     (suggest/suggester-keydown-handler! location (fake-key-event keycodes.ENTER))
     (rf/dispatch [:flaglib2.excerpt-search/accept-entry])

     ;;Reset - might not be needed?
     (rdom/render
      [es/excerpt-search
       :text text
       :on-change (fn [res] (reset! result res))]
      el)

     (rf/dispatch [:flaglib2.excerpt-search/do-search "Co-workers have" tdat])
     (suggest/suggester-keydown-handler! location (fake-key-event keycodes.ENTER))
     (rf/dispatch [:flaglib2.excerpt-search/accept-entry])

     (is (= nil @result))

     (is (= "Co-workers have" (nth @(rf/subscribe [:flaglib2.fabricate/excerpt]) 0))))))
