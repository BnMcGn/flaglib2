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
       :whitespace (reformat-whitespace-data (trampoline offwhite 0 []))})))

(defn contiguous-whitespace? [tdat index]
  (get-in tdat [:whitespace index] 0))

(defn some-excerpt-here? [tdat excerpt index]
  (let [[exdat excerpt] (if (string? excerpt) [(create-textdata excerpt) excerpt]
                            [excerpt (:text excerpt)])
        text (:text tdat)
        tlen (count text)
        elen (count excerpt)]
    (loop [tind index
           eind 0]
      (cond
        (= elen eind) {:remaining 0 :end-index tind}
        (= tlen tind) {:remaining (- elen eind) :end-index tind}
        :else
        (let [ewhite (contiguous-whitespace? exdat eind)
              twhite (contiguous-whitespace? tdat tind)]
          (if (and (zero? ewhite) (zero? twhite) (= (get excerpt eind) (get text tind)))
            (recur (+ 1 tind) (+ 1 eind))
            (if (or (zero? ewhite) (zero? twhite))
              {:remaining (+ 1 (- elen eind)):end-index (- tind 1)}
              (recur (+ tind twhite) (+ eind ewhite)))))))))


(defn excerpt-here? [tdat excerpt index]
  (let [res (some-excerpt-here? tdat excerpt index)]
    (if (= 0 (:remaining res))
      (:end-index res)
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



