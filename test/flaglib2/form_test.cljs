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

(deftest supply-text
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "supply-text"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Supply the article text and title"))
     (is (string/index-of text "<textarea"))
     (is (string/index-of text "<input"))
     (is (string/index-of text "Next")))))

(deftest review-text
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "review-text"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Review article text"))
     (is (string/index-of text "<textarea"))
     (is (string/index-of text "<input"))
     (is (string/index-of text "Reset"))
     (is (string/index-of text "Next")))))

(deftest opine-deluxe
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "opine-deluxe"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Article text options"))
     (is (string/index-of text "Custodial: Blank"))
     (is (string/index-of text "Reference: "))
     (is (string/index-of text "Post")))))

(deftest opine-bad-excerpt
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "opine-bad-excerpt"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "zmdi-alert-triangle")))))

(deftest target-return
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "target-return"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Article text options"))
     (is (string/index-of text "Custodial: Blank"))
     (is (string/index-of text "Reference: "))
     (is (string/index-of text "Next"))
     (is (not (string/index-of text "Post"))))))

(deftest target-adjustable
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "target-adjustable"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Adjusted to target"))
     (is (string/index-of text "Next (Accept as Entered)"))
     (is (string/index-of text "Next (Corrected Target)")))))

(deftest post-success
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "post-success"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Article text options"))
     (is (string/index-of text "Custodial: Blank"))
     (is (string/index-of text "Reference: "))
     (is (string/index-of text "Post"))
     (is (string/index-of text "Opinion posted"))
     (is (string/index-of text "Alternate title posted")))))

(deftest post-fail
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "post-fail"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Article text options"))
     (is (string/index-of text "Custodial: Blank"))
     (is (string/index-of text "Reference: "))
     (is (string/index-of text "Retry"))
     (is (string/index-of text "Opinion failed to post")))))

(deftest simple-vote
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "simple-vote"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Negative: Dislike"))
     (is (string/index-of text "Post")))))

(deftest simple-comment
  (rf-test/run-test-sync
   (rf/dispatch [::init/store-server-parameters nil {:section "simple-comment"}])
   (rf/dispatch [:mock-make])
   (let [el (js/document.createElement "div")
         _ (rdom/render [step/wf-stepper] el)
         text (. el -innerHTML)]
     (is (string/index-of text "Enter a Comment"))
     (is (string/index-of text "Post"))
     (is (not (string/index-of text "Target: ")))
     (is (not (string/index-of text "Article text options"))))))
