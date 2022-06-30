(ns flaglib2.excerpts-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [flaglib2.excerpts :as excerpts]
     ))


;;FIXME: need to test textdata creation

(def text1 "In the beginning was the Word, and the Word was with God, and the Word was God.
The same was in the beginning with God.
All things were made by him; and without him was not any thing made that was made.
In him was life; and the life was the light of men.
And the light shineth in darkness; and the darkness comprehended it not.")
(def tdat1 (excerpts/create-textdata text1))



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
    (is (every? (partial = 4) (map :remaining starts)))
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


(deftest single-match
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "prehenot")]
    (is (= 1 (count starts)))
    (is (= 1 (count ends)))))

(deftest no-match
  (let [[starts ends] (excerpts/excerpt-possibilities tdat1 "man")]
    (is (= 0 (count starts)))
    (is (= 0 (count ends)))))


