(ns flaglib2.macros)

(defmacro reg-json-fetch
  [[name-key url &
    {:keys [timeout attempts method proceed-test params-func]}]
   success-clause
   failure-clause]
  `(flaglib2.fetchers/generic-fetcher-events-generator
    ~name-key
    ~url
    ~(if success-clause
       `(cljs.core/fn ~@success-clause)
       '(cljs.core/fn [& _] {}))
    ~(if failure-clause
       `(cljs.core/fn ~@failure-clause)
       '(cljs.core/fn [result & _]
          (println "JSON-fetch failure:")
          (println result)))
    :response-format (ajax.core/json-response-format)
    ~@(when proceed-test `(:go? ~proceed-test))
    ~@(when params-func `(:params-func ~params-func))
    ~@(when timeout `(:timeout ~timeout))
    ~@(when attempts `(:attempts ~attempts))
    ~@(when method `(:method ~method))))
