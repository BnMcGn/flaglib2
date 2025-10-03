(ns flaglib2.excerpts-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [reagent.dom :as rdom]

     [clojure.string :as string]

     [flaglib2.excerpts :as excerpts]
     [flaglib2.hilited :as hilited]
     [flaglib2.misc :as misc]
     ))


;;FIXME: need to test textdata creation

(def text1 "In the beginning was the Word, and the Word was with God, and the Word was God.
The same was in the beginning with God.
All things were made by him; and without him was not any thing made that was made.
In him was life; and the life was the light of men.
And the light shineth in darkness; and the darkness comprehended it not.")
(def tdat1 (excerpts/create-textdata text1))

(deftest find-excerpt-position
  (let [[start plus] (excerpts/find-excerpt-position tdat1 "the beginning" :offset 1)]
    (is (= 13 plus))
    (is (= 96 start))))

(deftest find-excerpt-position-bad-offset
  (let [result (excerpts/find-excerpt-position tdat1 "the beginning" :offset 2)]
    (is (not result))))

(deftest excerpt-context
  (let [{:keys [leading trailing excerpt]} (excerpts/excerpt-context text1 96 13)]
    (is (= "the beginning" excerpt))
    (is (string/starts-with? trailing " with God"))
    (is (string/ends-with? leading "was in ")))
  (let [{:keys [leading trailing excerpt]} (excerpts/excerpt-context2 tdat1 "the beginning" 1)]
    (is (= "the beginning" excerpt))
    (is (string/starts-with? trailing " with God"))
    (is (string/ends-with? leading "was in ")))
  (is (thrown? js/Error
              (excerpts/excerpt-context2 tdat1 "man" 0))))

(deftest short-search
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "the")]
    (is (= 9 (count starts)))))

(deftest complete-excerpt
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "light of men")
        res (nth starts 0)]
    (is (= 1 (count starts)))
    (is (= 0 (:remaining res)))
    (is (= 241 (:start-index res)))
    (is (= 252 (:end-index res)))))

(deftest gapped-search
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "light the")
        res (nth starts 0)]
    (is (= 2 (count starts)))
    (is (every? (partial = 3) (map :remaining starts)))
    (is (= 241 (:start-index res)))
    (is (= 246 (:end-index res)))))

(deftest gapped-search-with-start
  (let [start {:remaining 4 :start-index 241 :end-index 246}
        [starts ends] (excerpts/excerpt-possibilities tdat1 "light the" start)]
    (is (= 1 (count starts)))
    (is (= 2 (count ends)))
    (is (every? (partial = 0) (map :remaining ends)))))

(deftest gapped-search-with-short-start
  (let [start {:remaining 4 :start-index 241 :end-index 243}
        [starts ends] (excerpts/excerpt-possibilities tdat1 "light the" start)]
    (is (= 1 (count starts)))
    (is (= 2 (count ends)))
    (is (every? (partial = 0) (map :remaining ends)))))

(deftest unspaced-gapped-search-with-short-start
  (let [start {:remaining 4 :start-index 241 :end-index 243}
        [starts ends] (excerpts/excerpt-possibilities tdat1 "lightthe" start)]
    (is (= 1 (count starts)))
    (is (= 2 (count ends)))
    (is (every? (partial = 0) (map :remaining ends)))))

(deftest bad-start
  (let [start {:remaining 4 :start-index 240 :end-index 243}]
    (is (thrown? js/Error
                 (excerpts/excerpt-possibilities tdat1 "light the" start)))))


(deftest double-search
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "light  the")
        res (nth starts 0)]
    (is (= 2 (count starts)))
    (is (every? (partial = 0) (map :remaining starts)))
    (is (= 241 (:start-index res)))
    (is (= 245 (:end-index res)))))

(deftest double-search-with-start
  (let [start {:remaining 0 :start-index 241 :end-index 245}
        [starts ends] (excerpts/excerpt-possibilities tdat1 "light  the" start)]
    (is (= 1 (count starts)))
    (is (= 2 (count ends)))
    (is (every? (partial = 0) (map :remaining ends)))))

(deftest double-search-with-short-start
  (let [start {:remaining 4 :start-index 241 :end-index 243}
        [starts ends] (excerpts/excerpt-possibilities tdat1 "light  the" start)]
    (is (= 1 (count starts)))
    (is (= 2 (count ends)))
    (is (every? (partial = 0) (map :remaining ends)))))

(deftest double-with-bad-start
  (let [start {:remaining 4 :start-index 240 :end-index 243}]
    (is (thrown? js/Error
                 (excerpts/excerpt-possibilities tdat1 "light  the" start)))))

(deftest simple-match-with-short-start
  (let [start {:remaining 0 :start-index 241 :end-index 243}
        [starts ends] (excerpts/excerpt-possibilities tdat1 "light of men" start)
        res (nth starts 0)]
    (is (= 1 (count starts)))
    (is (empty? ends))
    (is (= 0 (:remaining res)))
    (is (= 241 (:start-index res)))
    (is (= 252 (:end-index res)))))

(deftest single-match
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "prehenot")]
    (is (= 1 (count starts)))
    (is (= 1 (count ends)))))

(deftest no-match
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "man")]
    (is (= 0 (count starts)))
    (is (= 0 (count ends)))))


(deftest make-segments
  (let [rootkey "http://fake.fake/"
        iid "pnnkaeeeebaeebaeebaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebaeeeeb"
        db {:opinion-store {iid
                            {:excerpt "the"
                             :target rootkey}}
            :text-store {rootkey {:text text1}}}
        res (hilited/make-segments rootkey db [iid])
        [seg1 seg2 seg3] res
        [_ [_ text1]] (misc/part-on-true (partial = :text) seg1)
        [_ [_ text2]] (misc/part-on-true (partial = :text) seg2)]
    (is (= text1 "In "))
    (is (= text2 "the"))))

(def text2
  "This sentence contains\n\na double newline.")

(defn segmentz [text excerpt]
  (let [rootkey "http://fake.fake/"
        iid "pnnkaeeeebaeebaeebaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebaeeeeb"
        db {:opinion-store {iid
                            {:excerpt excerpt
                             :target rootkey}}
            :text-store {rootkey {:text text}}}
        res (hilited/make-segments rootkey db [iid])
        el (js/document.createElement "div")]
    (rdom/render (into [:div] res) el)
    el))

(defn el-children [elt]
  (into [] (array-seq (. elt -children))))

(defn get-range-at [node start end]
  (let [range (.. rangy (createRange js/document))]
    (. range (selectCharacters node start end))
    range))

(deftest select-excerpt-near-newline
  (let [el (segmentz text2 "a double")]
    (is (= "This sentence contains"
           (-> el
               el-children
               first
               el-children
               first
               (. -innerText))))
    (is (= "BR"
           (-> el
               el-children
               first
               el-children
               first
               el-children
               first
               (. -tagName))))
    (is (= "BR"
           (-> el
               el-children
               first
               el-children
               first
               el-children
               second
               (. -tagName))))
    (is (= "a double"
           (-> el
               el-children
               first
               el-children
               second
               (. -innerText))))
    (is (= " newline."
           (-> el
               el-children
               first
               el-children
               (nth 2)
               (. -innerText))))))


