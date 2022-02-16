(ns flaglib2.misc)

(defn encode-uri-component2 [uri]
  (let [ichars ":\",()/\\%?="]
    (apply str
           (map (fn [itm]
                  (if (= -1 (.indexOf ichars itm))
                    itm
                    (str "%" (.toUpperCase (.toString (.charCodeAt itm 0) 16)))))
                (seq uri)))))

