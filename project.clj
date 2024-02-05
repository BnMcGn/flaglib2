(defproject flaglib2 "1.9.0"
  :description "UI components for WarFlagger frontends"
  :url "https://github.com/BnMcGn/flaglib2"
  :license {:name "Apache License 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/clojurescript "1.11.132"]
                 [org.clojure/data.json "2.5.0"]
                 [cljsjs/react "17.0.2-0"]
                 [cljsjs/react-dom "17.0.2-0"]
                 [reagent "1.1.1"]
                 [re-frame "1.3.0-rc3"]
                 [day8.re-frame/http-fx "0.2.4"]
                 [com.andrewmcveigh/cljs-time "0.5.2"]
                 [cljsjs/fuse "6.0.0-0"]
                 [cljsjs/rangy-textrange "1.3.0-1"]
                 [rgm/tailwind-hiccup "0.2.0"]
                 [camel-snake-kebab "0.4.3"]
                 [org.clojars.bnmcgn/re-com-tailwind "2.13.2.1"]]

  :plugins [[lein-tailwind "0.1.2"]]

  :source-paths ["src"]

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "min"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "flaglib2.test-runner"]}

  :profiles {:dev {:dependencies [[com.bhauman/figwheel-main "0.2.16"]
                                  [org.slf4j/slf4j-nop "1.7.30"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"
                                   :exclusions [rewrite-cljs/rewrite-cljs]]
                                  [day8.re-frame/tracing      "0.6.2"]
                                  [day8.re-frame/re-frame-10x "1.9.6"
                                   :exclusions [org.clojure/tools.reader binaryage/devtools]]
                                  [day8.re-frame/test "0.1.5"]
                                  [com.bhauman/cljs-test-display "0.1.1"]]
                   :resource-paths ["target"]
                   ;; need to add the compiled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["target"]}}

  :tailwind {:tailwind-dir "."
             :output-dir   "resources/public/css"
             :tailwind-config  "tailwind.config.js" ;; tailwind.config.js is the default value
             :styles [{:src "style.src.css"
                       :dst "main.css"}]})

