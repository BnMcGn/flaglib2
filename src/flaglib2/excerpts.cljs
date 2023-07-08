(ns flaglib2.excerpts
  (:require
   [clojure.string :as string]
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
  (let [text (string/trim text)
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
                  {:remaining (- elen eind) :end-index (- tind 1)})
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
        (when (not-empty i)
          (if (integer? loc)
            (if (< 0 offset)
              (recur (rest i) (- offset 1))
              [(first i) (- loc (first i))])
            (recur (rest i) offset)))))))

(defn previous-break [text index]
  (string/last-index-of text \newline index))

(defn next-break [text index]
  (string/index-of text \newline index))

(defn excerpt-context [text position1 position2]
  (let [text (string/trim text)
        tlength (count text)
        estart position1
        eend (+ position2 estart)
        tstart (previous-break text estart)
        tend (next-break text eend)
        leading-context (subs text (if tstart (+ 1 tstart) 0) estart)
        excerpt (subs text estart eend)
        trailing-context (subs text eend (or tend tlength))]
    {:leading leading-context :trailing trailing-context :excerpt excerpt}))

(defn excerpt-context2 [tdat excerpt offset]
  (let [[p1 p2] (find-excerpt-position tdat excerpt :offset offset)]
    (excerpt-context (:text tdat) p1 (inc p2))))

(defn rebreak [text]
  (butlast
   (reduce
    into
    (map (fn [tx] [tx [:br]]) (string/split-lines text)))))

(defn overlap? [start1 end1 start2 end2]
  (not (or (> start1 end2) (> start2 end1))))

;;Find all the indices where excerpts start or stop.
(defn excerpt-segment-points [opset end]
  "End is the length of the text"
  (sort - (reduce into #{0 (+ 1 end)}
                  (for [itm opset
                        :let [[start end] (:text-position itm)]]
                    #{start (+ start end)}))))

(defn has-excerpt? [opin]
  (not (empty? (:excerpt opin))))

(defn has-found-excerpt? [opin]
  (and (has-excerpt? opin)
       (when-let [pos (:text-position opin)]
         (first pos))))

(defn clean-string-for-excerpt [the-string]
  (loop [res nil
         last-was-white false
         i (- (count the-string) 1)]
    (cond
      (> 0 i) (apply str res)
      (contains? misc/whitespace-characters (get the-string i))
      (if last-was-white
        (recur res true (- i 1))
        (recur (cons \space res) true (- i 1)))
      :else
      (recur (cons (get the-string i) res) false (- i 1)))))

(defn calculate-offset [tdat excerpt startloc]
  (if (zero? (count excerpt))
    nil
    (count (filter (fn [i] (excerpt-here? tdat excerpt i)) (range startloc)))))

;; Code for searching for excerpt.

;; Ideas:
;; - some sort of fuzzy search, so that user doesn't need to type out whole excerpt
;; - Don't show whole text, because of copyright concerns
;; - Start by assuming that first part of input should match start of excerpt.
;;  - Order by largest match, or by first match in document order?
;;  - If we have multiple matches, present them. User may select one.
;;  - If user selects, or we are down to one, move on to selecting end.
;;  - Minimum starting size, perhaps 5 characters. Must be contiguous. What about short excerpts?

(defn find-possible-excerpt-ends [tdat end-of-start remainder]
  "Find possible endings for an excerpt when start is already known.
Decide before calling where the start has ended. Will return some-excerpt-here? style matches"
  (let [excerpt (create-textdata remainder)]
    (for [i (range end-of-start (:text-length tdat))
          :let [match (some-excerpt-here? tdat excerpt i)]
          :when (and match (= (:remaining match) 0))]
      (assoc match :start-index i))))

(defn has-viable-excerpt-end? [tdat end-of-start remainder]
  (when remainder
    (not-empty (find-possible-excerpt-ends tdat end-of-start remainder))))

(defn remaining-portion-of-search [search res]
  (let [slen (count search)
        rindex (- slen (:remaining res))]
    (when (pos? rindex)
      (subs search rindex))))

(defn find-possible-excerpt-starts [tdat excerpt]
  (let [excerpt (create-textdata excerpt)
        ;;Don't consider starts shorter than this, but return complete matches
        minimum (min 3 (:text-length excerpt))]
    (for [i (range (:text-length tdat))
          :let [match (some-excerpt-here? tdat excerpt i)]
          :when (and match
                     (or (zero? (:remaining match))
                         (and (>= (- (:text-length excerpt) (:remaining match)) minimum)
                              (has-viable-excerpt-end?
                               tdat (:end-index match)
                               (remaining-portion-of-search (:text excerpt) match)))))]
      (assoc match :start-index i))))

(defn length-of-match [match]
  (+ 1 (- (:end-index match) (:start-index match))))

(defn split-search-on-double-space [srch]
  ;;FIXME: is trim right thing to do?
  (let [srch (string/trim srch)
        res (string/index-of srch "  ")]
    (if res
      [(subs srch 0 res) (subs srch (+ res 2))]
      [srch])))

(defn ensure-correct-start [tdat search start]
  (let [[seg1 seg2] (split-search-on-double-space search)
        stindex (:start-index start)]
    (if seg2
      (if (excerpt-here? tdat seg1 stindex)
        (if (= (length-of-match start) (count seg1))
          start
          {:start-index stindex :remaining 0 :end-index (+ stindex (count seg1))})
        (throw (js/Error. "Bad match start. Shouldn't happen!")))
      ;;We could check the old search for correctness, but this is simpler
      (if-let [res (some-excerpt-here? tdat search stindex)]
        (assoc res :start-index stindex)
        (throw (js/Error. "Bad match start. Shouldn't happen!"))))))

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
         (if (= (count search) (length-of-match (nth res 0)))
           [res res] ;;FIXME: is this correct?
           (excerpt-possibilities tdat search (nth res 0))))
       :else [res '()])))
  ;;Handle end search
  ([tdat search found-start]
   (let [[seg1 seg2] (split-search-on-double-space search)
         start (ensure-correct-start tdat search found-start)
         seg2 (or seg2
                  (remaining-portion-of-search search start))]
     (if seg2
       [(list start) (find-possible-excerpt-ends tdat (:end-index start) seg2)]
       ['() '()]))))

(defn excerpt-start-valid? [tdat search start]
  (let [i (:start-index start)
        match (some-excerpt-here? tdat search i)]
    (when match
      (>= (:end-index match) (:end-index start)))))

(defn start-end->excerpt-offset [tdat start end]
  (let [end-index (if end (:end-index end) (:end-index start))
        excerpt (subs (:text tdat) (:start-index start) (inc end-index))]
    [(clean-string-for-excerpt excerpt)
     (calculate-offset tdat excerpt (:start-index start))]))

(defn excerpt-offset->start [tdat excerpt offset]
  (let [res (find-excerpt-position tdat excerpt :offset offset)]
    (when res
      {:start-index (res 0) :end-index (+ (res 0) (res 1)) :remaining 0})))


