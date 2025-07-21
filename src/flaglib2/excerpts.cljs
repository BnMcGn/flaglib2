(ns flaglib2.excerpts
  (:require
   [clojure.string :as string]
   [clojure.set :as set]

   [re-frame.alpha :as rf]

   [flaglib2.misc :as misc]
   [flaglib2.subscriptions :as subs]))


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
        (+ 1 (:end-index res))
        false)
      false)))

(defn find-excerpt-position [tdat excerpt & {:keys [offset] :or {offset 0}}]
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
  (when-not (and (integer? position1) (integer? position2))
    (throw (js/Error. "Position vars must be integers")))
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
    (if p1
      (excerpt-context (:text tdat) p1 p2)
      (throw (js/Error. "No excerpt found")))))

(defn rebreak [text]
  (into
   [:<>]
   (butlast
    (reduce
     into
     (map (fn [tx] [tx [:br]]) (string/split-lines text))))))

(defn overlap? [start1 end1 start2 end2]
  (not (or (> start1 end2) (> start2 end1))))

;;Find all the indices where excerpts start or stop.
(defn excerpt-segment-points
  "End is the length of the text"
  [opset end]
  (sort - (reduce into #{0 (+ 1 end)}
                  (for [itm opset
                        :let [[start end] (:text-position itm)]]
                    #{start (+ start end)}))))

(defn has-excerpt? [opin]
  (not (empty? (:excerpt opin))))

;; Is the original text position available and applicable?
;; If not, generate a new one when possible.
(defn recalc-text-position [db key]
  (when (misc/iid? key)
    (let [opinion (get-in db [:opinion-store key])
          {:keys [target text-position excerpt offset]} opinion]
      (when (has-excerpt? opinion)
        (if (misc/iid? target)
          (if (and text-position (integer? (first text-position)))
            :original
            (find-excerpt-position
             (create-textdata (subs/proper-text db target)) excerpt :offset offset))
          (let [tinfo (get-in db [:text-store target])]
            (if (and (= :initial (:text-source tinfo))
                     (and text-position (integer? (first text-position))))
              :original
              (find-excerpt-position
               (create-textdata (subs/proper-text db target))
               excerpt :offset offset))))))))

(rf/reg-sub
 :text-position-recalc
 :<- [:core-db]
 (fn [db [_ key]]
   (recalc-text-position db key)))

;;Can an opinion be considered 'bookmark' for an excerpt? As such it might serve as a target
;; for inrefs. If so, it should look like a utility opinion.
(defn qualifies-as-excerpt-marker? [opin]
  (and (has-excerpt? opin)
       (= :custodial-blank (:flag opin))
       (or (not (:comment opin)) (empty? (:comment opin)))))

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

(defn get-location-excerpt [tdat start end]
  (let [excerpt (subs (:text tdat) start end)
        excerpt (clean-string-for-excerpt excerpt)
        offset (calculate-offset tdat excerpt start)]
    {:excerpt excerpt :offset offset}))

;; Need an accurate location for excerpt

(defn- anchor-wrapper? [node]
  (when-let [att node.attributes]
    (when-let [rcdat (. att (getNamedItem "data-rc"))]
      (= "popover-anchor-wrapper" rcdat.value))))

(defn- is-tag? [tag el]
  (or (and el
           el.tagName
           (= el.tagName tag))
      false))

;;FIXME: Not very safe. Should identify span, throw error if not found
(defn- get-actual-span [span]
  (if (anchor-wrapper? span) span.firstChild.firstChild span))

;; Emits a lazyseq of text and <br> nodes, omitting the non-article stuff
(defn hilited-node-walk [el]
  (remove (partial is-tag? "SPAN")
          (mapcat identity
                  (map
                   #(-> %1
                        get-actual-span
                        (. -childNodes)
                        seq)
                   (seq (. el -childNodes))))))

(defn- proc-in-excerpt [nodes range]
  (let [node (first nodes)
        text (if (is-tag? "BR" node) "\n" (and node node.data))]
    (cond
      (. node isEqualNode (. range -endContainer)) [(subs text 0 (. range -endOffset))]
      text (lazy-seq (cons text (proc-in-excerpt (next nodes) range)))
      :else [:fail])))

(defn- proc-excerpt-start [nodes range]
  (let [node (first nodes)
        offset (. range -startOffset)
        text (if (is-tag? "BR" node) "\n" node.data)]
    (if (. node isEqualNode (. range -endContainer))
      [(subs text 0 offset) :marker (subs text offset (. range -endOffset))]
      (lazy-cat
       [(subs text 0 offset) :marker (subs text offset)]
       (proc-in-excerpt (next nodes) range)))))

(defn- proc-pre-excerpt [nodes range]
  (let [node (first nodes)]
    (cond
      (not node) [:fail]
      (. node isEqualNode (. range -startContainer)) (proc-excerpt-start nodes range)
      (is-tag? "BR" node) (lazy-seq (cons "\n" (proc-pre-excerpt (next nodes) range)))
      (and node.nodeName (= "#text" node.nodeName))
      (lazy-seq (cons node.data (proc-pre-excerpt (next nodes) range)))
      :else [:fail])))

(defn text-location-from-dom-range
  "el is presumed to be a hilited-text div"
  [el range]
  (let [segments (proc-pre-excerpt (hilited-node-walk el) range)
        [pre excerpt] (split-with string? segments)]
    (when-not (or (= :fail (first excerpt)) (= :fail (last excerpt)))
      (let [start (reduce + (map count pre))
            excerpt (apply str (rest excerpt))
            length (count excerpt)]
        {:start start :end (+ length start) :excerpt excerpt :length length}))))

;; Code for searching for excerpt.

;; Ideas:
;; - some sort of fuzzy search, so that user doesn't need to type out whole excerpt
;; - Don't show whole text, because of copyright concerns
;; - Start by assuming that first part of input should match start of excerpt.
;;  - Order by largest match, or by first match in document order?
;;  - If we have multiple matches, present them. User may select one.
;;  - If user selects, or we are down to one, move on to selecting end.
;;  - Minimum starting size, perhaps 5 characters. Must be contiguous. What about short excerpts?

(defn find-possible-excerpt-ends
  "Find possible endings for an excerpt when start is already known.
Decide before calling where the start has ended. Will return some-excerpt-here? style matches"
  [tdat end-of-start remainder]
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
        ;;Some results don't have a :start-index, others don't have a reliable :remaining
        rindex (if (:start-index res)
                 (+ 1 (- (:end-index res) (:start-index res)))
                 (- slen (:remaining res)))]
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
   (let [[_ seg2] (split-search-on-double-space search)
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

(defn search-tail-is-whitespace? [search start]
  (let [tail (remaining-portion-of-search search start)]
    (when-not (empty? tail)
      (set/subset? (set tail) (set misc/whitespace-characters)))))

(defn start-end->excerpt-offset [tdat start end]
  (let [end-index (if end (max (:end-index end) (:end-index start)) (:end-index start))
        excerpt (subs (:text tdat) (:start-index start) (inc end-index))]
    [(clean-string-for-excerpt excerpt)
     (calculate-offset tdat excerpt (:start-index start))]))

(defn excerpt-offset->start [tdat excerpt offset]
  (let [res (find-excerpt-position tdat excerpt :offset offset)]
    (when res
      {:start-index (res 0) :end-index (+ (res 0) (res 1) -1) :remaining 0})))


