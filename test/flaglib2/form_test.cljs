(ns flaglib2.form-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [day8.re-frame.test :as rf-test]
     [clojure.string :as string]

     [flaglib2.mock-make :as mm]
     [flaglib2.init :as init]
     [flaglib2.stepper :as step]
     [flaglib2.misc :as misc]

     [reagent.dom :as rdom]
     [re-frame.alpha :as rf]
     ))

(deftest opine
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "opine"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Choose an Excerpt"))
     (is (string/index-of text "Set a Reference"))
     (is (string/index-of text "Post")))))

(deftest decision-reviewed
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "decision-reviewed"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Next</button")))))

(deftest decision-available
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "decision-available"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "not yet been reviewed"))
     (is (string/index-of text "Skip to Flagging"))
     (is (string/index-of text "Review Text")))))

(deftest decision-wait
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "decision-wait"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Waiting for text extraction"))
     (is (string/index-of text "Supply Text")))))

(deftest decision-failure
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "decision-failure"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)
         dec @(rf/subscribe [:target-decision])]
     (is (string/index-of text "<h4>Reason"))
     (is (string/index-of text "Supply Text"))

     ;;These are just defaults, not what would be here in actual use
     (is (= :wait (:status dec)))
     (is (not (:message dec))))))


