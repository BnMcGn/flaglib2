(ns flaglib2.excerpts
  (:require
   [clojure.string :as str]
   [flaglib2.misc :as misc]))


;;; Excerpt and text tools


(defn- reformat-whitespace-data [data]
  (into {}
        (for [group data
              itm (map (fn [a b] [a b])
                       group
                       (range (count group) 0 -1))]
          itm)))

(defn create-textdata [text]
  ;;FIXME: trim not customized. Might not match other ws definitions
  (let [text (str/trim text)
        tlen (count text)]
    (letfn
        [(white [i stor]
           (let [ct (misc/first-index nil (subs text i)
                                      :test #(not (contains? misc/whitespace-characters %1)))]
             #(offwhite (+ i ct) (into stor [(range i (+ ct i))]))))
         (offwhite [i stor]
           (cond
             (>= i tlen) stor
             (contains? misc/whitespace-characters (get text i)) #(white i stor)
             :else #(offwhite (+ 1 i) stor)))]
      {:text text
       :text-length tlen
       :whitespace (reformat-whitespace-data (trampoline offwhite 0 []))})))

(defn contiguous-whitespace? [tdat index]
  (get-in tdat [:whitespace index] 0))

(defn some-excerpt-here? [tdat excerpt index]
  (let [[exdat excerpt] (if (string? excerpt) [(create-textdata excerpt) excerpt]
                            [excerpt (:text excerpt)])
        text (:text tdat)
        tlen (:text-length tdat)
        elen (count excerpt)]
    (if (zero? elen)
      false
      (loop [tind index
             eind 0]
        (cond
          (= elen eind) {:remaining 0 :end-index (- tind 1)}
          (= tlen tind) {:remaining (- elen eind) :end-index (- tind 1)}
          :else
          (let [ewhite (contiguous-whitespace? exdat eind)
                twhite (contiguous-whitespace? tdat tind)]
            (if (and (zero? ewhite) (zero? twhite) (= (get excerpt eind) (get text tind)))
              (recur (+ 1 tind) (+ 1 eind))
              (if (or (zero? ewhite) (zero? twhite))
                (if (zero? eind)
                  false
                  {:remaining (+ 1 (- elen eind)) :end-index (- tind 1)})
                (recur (+ tind twhite) (+ eind ewhite))))))))))

(defn excerpt-here? [tdat excerpt index]
  (let [res (some-excerpt-here? tdat excerpt index)]
    (if res
      (if (= 0 (:remaining res))
       (:end-index res)
       false)
      false)))

(defn find-excerpt-position [tdat excerpt & {:keys [offset] :or {:offset 0}}]
  (let [exdat (create-textdata excerpt)]
    (loop [i (range (count (:text tdat)))
           offset offset]
      (let [loc (excerpt-here? tdat exdat (first i))]
        (when i
          (if (integer? loc)
            (if (< 0 offset)
              (recur (rest i) (- offset 1))
              [(first i) (- loc (first i))])
            (recur (rest i) offset)))))))

(defn previous-break [text index]
  (str/last-index-of text \newline index))

(defn next-break [text index]
  (str/index-of text \newline index))

(defn excerpt-context [text position1 position2]
  (let [text (str/trim text)
        tlength (count text)
        estart position1
        eend (+ position2 estart)
        tstart (previous-break text estart)
        tend (next-break text eend)
        leading-context (subs text (if tstart (+ 1 tstart) 0) estart)
        excerpt (subs text estart eend)
        trailing-context (subs text eend (or tend tlength))]
    {:leading leading-context :trailing trailing-context :excerpt excerpt}))


;; Code for searching for excerpt.

;; Ideas:
;; - some sort of fuzzy search, so that user doesn't need to type out whole excerpt
;; - Don't show whole text, because of copyright concerns
;; - Start by assuming that first part of input should match start of excerpt.
;;  - Order by largest match, or by first match in document order?
;;  - If we have multiple matches, present them. User may select one.
;;  - If user selects, or we are down to one, move on to selecting end.
;;  - Minimum starting size, perhaps 5 characters. Must be contiguous. What about short excerpts?

(defn find-possible-excerpt-starts [tdat excerpt]
  (let [excerpt (create-textdata excerpt)
        ;;Don't consider starts shorter than this, but return complete matches
        minimum (min 5 (:text-length excerpt))]
    (for [i (range (:text-length tdat))
          :let [match (some-excerpt-here? tdat excerpt i)]
          :when (and match
                     (or (zero? (:remaining match))
                         (> (- (:text-length excerpt) (:remaining match)) minimum)))]
      (assoc match :start-index i))))

(defn find-possible-excerpt-ends [tdat end-of-start remainder]
  "Find possible endings for an excerpt when start is already known.
Decide before calling where the start has ended. Will return some-excerpt-here? style matches"
  (let [excerpt (create-textdata remainder)]
    (for [i (range end-of-start (:text-length tdat))
          :let [match (some-excerpt-here? tdat excerpt i)]
          :when (and match (= (:remaining match) 0))]
      (assoc match :start-index i))))

(defn length-of-match [match]
  (+ 1 (- (:end-index match) (:start-index match))))

(defn split-search-on-double-space [srch]
  ;;FIXME: is trim right thing to do?
  (let [srch (str/trim srch)
        res (str/index-of srch "  ")]
    (if res
      [(subs srch 0 res) (subs srch (+ res 2))]
      [srch])))

(defn remaining-portion-of-search [search res]
  (let [slen (count search)
        rindex (- slen (:remaining res))]
    (when (pos? rindex)
      (subs search rindex))))

(defn excerpt-possibilities
  ([tdat search]
   (let [[seg1 seg2] (split-search-on-double-space search)
         res (find-possible-excerpt-starts tdat seg1)
         resc (count res)]
     (cond
       (zero? resc) ['() '()]
       (= 1 resc)
       (if seg2
         (if (= (count seg1) (length-of-match (nth res 0)))
           (excerpt-possibilities tdat search (nth res 0))
           ['() '()])
         (excerpt-possibilities tdat search (nth res 0)))
       :else [res '()])))
  ;;Handle end search
  ([tdat search found-start]
   (let [[seg1 seg2] (split-search-on-double-space search)
         seg2 (or seg2
                  (remaining-portion-of-search search found-start))]
     (if seg2
       [[found-start] (find-possible-excerpt-ends tdat (:end-index found-start) seg2)]
       ['() '()]))))



