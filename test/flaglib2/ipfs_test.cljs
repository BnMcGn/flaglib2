(ns flaglib2.ipfs-test
    (:require
     [cljs.test :refer-macros [deftest is testing async]]
     [day8.re-frame.test :as rf-test]

     [clojure.string :as string]
     [goog.string :as gstring]
     [goog.string.format]

     [re-frame.alpha :as rf]
     ;[re-frame.flow.alpha :as flow]

     [flaglib2.ipfs :as ipfs]
     [flaglib2.misc :as misc]
     ))



(defn tid [num]
  (str "pnnkaeeeebaeebaeebaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebaeee"
       (.padStart (str num) 2 "0")))


(def rooturl "http://fake.fake/")

(def created "2020-03-16T06:29:59+0000")

(def opin-template
  ";;OpinML 0.0.1 :s-expression
  (:target \"%s\" :rooturl \"%s\" :flag %s  :comment \"%s\" :created \"%s\" :tree-address %s
  :iid \"%s\" :clean-comment \"%s\" %s)")

(defn make-string-opinion
  [& {:keys [iid target rooturl flag comment created tree-address clean-comment reference]}]
  (let [reference (and reference (gstring/format ":reference \"%s\"" reference))
        params [target rooturl flag comment created tree-address iid clean-comment reference]
        params (map #(or % "") params)]
    (apply (partial gstring/format opin-template) params)))

(def opinions
  [(make-string-opinion
    :iid (tid 0) :target rooturl :rooturl rooturl :flag '(:positive :like)
    :tree-address (list (tid 0)) :created created)
   (make-string-opinion
    :iid (tid 1) :target (tid 0) :rooturl rooturl :flag '(:positive :evidence)
    :tree-address (list (tid 0) (tid 1)) :reference "http://totally.fake/" :created created)
   (make-string-opinion
    :iid (tid 2) :target "http://looks.fake/" :rooturl "http://looks.fake/"
    :flag '(:negative :evidence) :tree-address (list (tid 2))
    :reference (tid 0) :created created)])

(def optree
  (gstring/format "((\"%s\" (\"%s\")))" (tid 0) (tid 1)))

(def warstat-template
  "(:replies-immediate 0 :replies-total 0 :tree-freshness
     \"2020-03-16T06:29:59+0000\" :x-right 0 :x-wrong 0 :x-up 0 :x-down 0 :effect 1
      :controversy 0 :hashtags nil :x-right-source nil :x-up-source nil
      :x-wrong-source nil :x-down-source nil :direction :neutral :direction-on-root
      :neutral :tt-thread nil)")

(def references
  (gstring/format
   "(:references (\"%s\") :reference-opinions (\"%s\") :refd (\"%s\"))"
    "http://totally.fake/" (tid 1) (tid 2)))

(def text
  "(:text-source \"pnnkaeeeebaeebaeebaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebaeeex\"
 :text
  \"This might be a page text, but probably not.\"
 :competitors
  (\"http://fake.fake/\"
  \"pnnkaeeeebaeebaeebaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebaeeey\")
  :initial-message \"INFO - Crawly: url-search 0 results from COMMON-CRAWL
WARN - Common Crawl: URL not found
INFO - Crawly: url-search 1 result from INTERNET-ARCHIVE
WARN - Internet Archive: unable to fetch page
ERROR - Page not available for extraction
  \"
  :initial-status \"failure\" :direction-on-root :neutral :direction :neutral
 :x-down-source nil :x-wrong-source nil :x-up-source nil :x-right-source nil
 :hashtags nil :controversy 0 :effect 1 :x-down 0 :x-up 0 :x-wrong 0 :x-right 0
  :tree-freshness \"2020-03-16T06:29:59+0000\" :replies-total 0 :replies-immediate
 0)")

(deftest load-data
  (rf-test/run-test-sync
   (binding [ipfs/http-xhrio :fake]
     (rf/dispatch [::ipfs/received-text rooturl text])
     (rf/dispatch [::ipfs/received-warstats rooturl warstat-template])
     (rf/dispatch [::ipfs/received-references rooturl references])
     (rf/dispatch [::ipfs/received-opinion-tree rooturl optree])
     (rf/dispatch [::ipfs/received-opinion (tid 0) (nth opinions 0)])
     (rf/dispatch [::ipfs/received-opinion (tid 1) (nth opinions 1)])
     (rf/dispatch [::ipfs/received-opinion (tid 2) (nth opinions 2)])
     (rf/dispatch [::ipfs/received-warstats (tid 0) warstat-template])
     (rf/dispatch [::ipfs/received-warstats (tid 1) warstat-template])
     (rf/dispatch [::ipfs/received-warstats (tid 2) warstat-template])
     (rf/dispatch [::ipfs/complete-debounce])
     (let [rtext @(rf/subscribe [:text-store rooturl])
           op0 @(rf/subscribe [:opinion-store (tid 0)])
           ws0 @(rf/subscribe [:warstats-store (tid 0)])
           refs @(rf/subscribe [:references rooturl])
           rootrefd @(rf/subscribe [:refd rooturl])
           oprefd @(rf/subscribe [:refd (tid 0)])]
       (is (string? (:text rtext)))
       (is (= "years" (second (misc/ago (:created op0)))))
       (is (= "years" (second (misc/ago (:tree-freshness ws0)))))
       (is (= (tid 2) (first rootrefd)))
       (is (= (tid 2) (first oprefd)))
       (is (= "http://totally.fake/" (first refs)))))))


