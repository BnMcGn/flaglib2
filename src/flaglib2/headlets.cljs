(ns flaglib2.headlets
  (:require
   [re-frame.core :as rf]
   [goog.object :as go]

   [flaglib2.misc :as misc]))




;; Head tools

(defn get-meta-elements []
  (let [head (first (seq (. js/document (getElementsByTagName "head"))))]
    (seq (. head (getElementsByTagName "meta")))))

(defn get-meta-map-by-attribute [attribute & elements]
  (let [elements (or elements (get-meta-elements))]
    (into {}
          (for [e elements
                :let [prop (. e (getAttribute attribute))]
                :when prop]
            [prop e]))))

(defn add-meta-tag! [el]
  (let [head (first (seq (. js/document (getElementsByTagName "head"))))]
    (. head (appendChild el))))

(defn set-meta-property! [property content]
  (if-let [el ((get-meta-map-by-attribute "property") property)]
    (. el (setAttribute "content" content))
    (let [el (. js/document (createElement "meta"))]
      (. el (setAttribute "property" property))
      (. el (setAttribute "content" content))
      (add-meta-tag! el))))

(defn set-meta-name! [name content]
  (if-let [el ((get-meta-map-by-attribute "name") name)]
    (. el (setAttribute "content" content))
    (let [el (. js/document (createElement "meta"))]
      (. el (setAttribute "name" name))
      (. el (setAttribute "content" name))
      (add-meta-tag! el))))

(rf/reg-fx
 :set-page-title
 (fn [new-title]
   (set! js/document.title new-title)))

(rf/reg-fx
 :set-opinion-meta
 (fn [opinion]
   (set-meta-property! "opinml:opinion" (:opinion opinion))
   (set-meta-property! "opinml:rooturl" (:rooturl opinion))
   (set-meta-property! "opinml:target" (:target opinion))))

(rf/reg-fx
 :set-social-meta
 (fn [{:keys [title description id]}]
   (set-meta-property! "og:title" title)
   (set-meta-property! "og:type" "article")
   (set-meta-property! "og:description" description)
   (set-meta-property! "og:image" (misc/make-social-image-url id))
   (set-meta-property! "og:url" (str (go/get js/window "SERVER")
                                     (if (misc/iid? id)
                                       (misc/make-opinion-url id)
                                       (misc/make-target-url id))))
   (set-meta-name! "twitter:card" "summary_large_image")))

