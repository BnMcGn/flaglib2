(ns flaglib2.mock-make
  (:require
   [re-frame.core :as rf]

   [flaglib2.forms :as forms]
   [flaglib2.stepper :as step]
   [flaglib2.misc :as misc]

   ))


(def plain-db
  {:flaglib2.posters/alternate-response nil,
   :title-store {},
   :opinion-store {},
   :flaglib2.fabricate/specify-target {:on-select nil, :suppress-search-results false},
   :flaglib2.posters/opinion-failure nil,
   :text-store {},
   :warstats-store {},
   :flaglib2.posters/opinion-response nil,
   :flaglib2.posters/alternate-failure nil,
   :flaglib2.fetchers/author-urls {}
   :local {:advanced true}})

(def sample-text "Sample Web Page

A random photo, maximize your browser to enlarge.
Frank da Cruz

Sat Jan 17 12:07:32 2004

* Creating a Web Page
* HTML Syntax
* Special Characters
* Converting Plain Text to HTML
* Effects
* Lists
* Links
* Tables
* Installing your Web Page on the Internet
* Where to go from here

This page was typed by hand. Anybody can do this, you don't need any
  special \"web creation\" tools or HTML editors, and the pages you make can be
viewed from any browser. To see how this page was made, chooseView Source (or View Page Source, or View Document Source) in your
browser's menu. A simple web page like this one is just plain text with
HTML commands (markup) mixed in. HTML commands themselves are plain text.
When you're just learning and want to experiment, you can do everything on
  your PC. Create a new directory (\"folder\") for your website, and then put
the web-page files (HTML plus any pictures) in it. Use NotePad or other
plain-text editor (not word processor) on your PC to create anindex.html file, which you can view locally with your Web browser.
(You can also use word processors such as Word or WordPad if you save in
  \"plain text\", \"text\", \"text document\", or \"text document MS-DOS format\".)Later I'll explain how you can install your web site on
the Internet.

  ... ")

(def title-store
  {"http://www.columbia.edu/~fdc/sample.html"
   {:x-up 1, :x-down 1, :x-wrong 0, :x-right 0,
    :initial-status "failure",
    :controversy 4,
    :title "Sample Web Page",
    :title-source :initial,
    :replies-immediate 0,
    :tree-freshness "2021-02-01T14:53:58+0000",
    :initial-message "Failed to load URL",
    :replies-total 8,
    :hashtags nil, :effect 0,
    :direction :neutral,
    :direction-on-root :neutral}})

(def text-store
  {"http://www.columbia.edu/~fdc/sample.html"
   {:x-up 1, :x-down 1, :x-wrong 0, :x-right 0,
    :initial-status "failure",
    :controversy 4,
    :text sample-text,
    :text-source :initial,
    :replies-immediate 0,
    :tree-freshness "2021-02-01T14:53:58+0000",
    :initial-message "Failed to load URL",
    :replies-total 8,
    :hashtags nil, :effect 0,
    :direction :neutral,
    :direction-on-root :neutral}})

(def warstats-store
  {"http://www.columbia.edu/~fdc/sample.html"
   {:x-up 1, :x-down 1, :x-wrong 0, :x-right 0,
    :controversy 4,
    :replies-immediate 0,
    :tree-freshness (misc/parse-time "2021-02-01T14:53:58+0000"),
    :replies-total 8,
    :hashtags nil, :effect 0,
    :direction :neutral,
    :direction-on-root :neutral}})

(def target-url "http://www.columbia.edu/~fdc/sample.html")

(def specify-target
  {:on-select nil,
   :suppress-search-results true,
   :flaglib2.urlgrab/search target-url,
   :flaglib2.urlgrab/selection target-url})

(def warstats-store-unreviewed
  (assoc-in warstats-store [target-url :replies-total] 0))

(def text-store-unavailable
  (update-in text-store [target-url] dissoc :text))

(def text-failed {target-url {:status "failure" :message "Failure is happened!!"}})

(def targetted-db
  (merge
   plain-db
   {:title-store title-store
    :text-store text-store
    :warstats-store warstats-store
    :flaglib2.fabricate/specify-target specify-target}))

(def reference-url "http://google.com/")

(def extra-db
  (merge
   targetted-db
   {:warstats-store warstats-store-unreviewed
    :flaglib2.fabricate/review-text "Adjusted text"
    :flaglib2.fabricate/flag :custodial-blank
    :flaglib2.fabricate/excerpt ["just" 0]
    :flaglib2.fabricate/specify-reference {:on-select nil,
                                           :suppress-search-results true,
                                           :flaglib2.urlgrab/search reference-url,
                                           :flaglib2.urlgrab/selection reference-url}
    :flaglib2.fabricate/comment "This might be a comment"}))

(def server-success
  {:flaglib2.posters/opinion-status {:response {:success true}}
   :flaglib2.posters/alternate-status {:response {:success true}}
   :flaglib2.posters/alt-title-status {:response {:success true}}})

(def server-fail
  {:flaglib2.posters/opinion-status {:failure {:status-text "Didn't make it"}}
   :flaglib2.posters/alternate-status {:response {:success true}}
   :flaglib2.posters/alt-title-status {:response {:errors {:comment "Too wrong"
                                                           :target "Don't look there!"}
                                                  :failure nil}}})

(def sections
  {:initial plain-db
   :opine targetted-db
   :decision-reviewed targetted-db
   :decision-available (assoc targetted-db :warstats-store warstats-store-unreviewed)
   :decision-wait (assoc targetted-db
                         :warstats-store warstats-store-unreviewed
                         :text-store text-store-unavailable)
   :decision-failure (assoc targetted-db
                            :warstats-store warstats-store-unreviewed
                            :text-store text-store-unavailable
                            :text-status text-failed)
   :supply-text (assoc targetted-db
                       :warstats-store warstats-store-unreviewed
                       :text-store text-store-unavailable
                       :text-status text-failed)
   :review-text (assoc targetted-db
                       :warstats-store warstats-store-unreviewed)
   :opine-deluxe extra-db
   :opine-bad-excerpt (assoc extra-db
                             :flaglib2.fabricate/excerpt ["nonexist" 0]
                             :flaglib2.excerpt-search/raw-excerpt-search "nonexist")
   :target-return extra-db
   :post-success (merge extra-db server-success)
   :post-fail (merge extra-db server-fail)
   :simple-vote (-> targetted-db
                    (assoc-in [:server-parameters :default :flag] :negative-dislike)
                    (assoc :local {:advanced false}))
   :simple-comment (-> targetted-db
                       (assoc-in [:server-parameters :default :flag] :custodial-blank)
                       (assoc :local {:advanced false}))})

(def section-step {:opine :opine
                   :decision-reviewed :target-decision
                   :decision-available :target-decision
                   :decision-wait :target-decision
                   :decision-failure :target-decision
                   :supply-text :supply-text
                   :review-text :review-text
                   :opine-deluxe :opine
                   :opine-bad-excerpt :opine
                   :target-return :specify-target
                   :post-success :opine
                   :post-fail :opine
                   :simple-vote :confirm
                   :simple-comment :opine})

;;Lists of steps that, per page, should be set to summary mode
(def summary-settings {:opine-deluxe [:specify-target :target-decision :review-text]
                       :opine-bad-excerpt [:specify-target :target-decision :review-text]
                       :target-return [:specify-target :target-decision :review-text
                                       :excerpt :reference :flag :opine]
                       :post-success [:specify-target :target-decision :review-text]
                       :post-fail [:specify-target :target-decision :review-text]})


(defn mock-make [_]
  [step/wf-stepper])

(rf/reg-event-fx
 :mock-make
 (fn [{:keys [db]} _]
   (let [params (-> db :server-parameters :default)
         section-name (keyword (:section params))
         section (get sections section-name)
         params (merge params (-> section :server-parameters :default))
         db (merge
             db
             section
             {:root-element mock-make
              :server-parameters {:default params}})
         summaries (for [step (get summary-settings section-name)]
                     [:dispatch [:flaglib2.stepper/set-summary step]])
         [steps _] (forms/what-opin-form? db)]
     (when-not section (throw (js/Error. "Mockable not found")))
     {:db db
      :fx (into
           [
            ;;[:dispatch [:add-hooks fabricate-hooks]]
            [:dispatch [:flaglib2.stepper/initialize steps]]
            (when-let [step (get section-step section-name)]
              [:dispatch [:flaglib2.stepper/goto step]])

            [:dispatch [:mount-registered]]]
           summaries)})))



