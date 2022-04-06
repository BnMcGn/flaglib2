(defproject flaglib2 "1.9.0"
  :description "UI components for WarFlagger frontends"
  :url "https://github.com/BnMcGn/flaglib2"
  :license {:name "Apache License 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.773"]
                 [reagent "0.10.0" ]
                 [re-frame "1.3.0-rc3"]
                 [day8.re-frame/http-fx "0.2.4"]
                 [com.andrewmcveigh/cljs-time "0.5.2"]
                 [cljsjs/fuse "6.0.0-0"]
                 [rgm/tailwind-hiccup "0.2.0"]
                 [re-com "2.13.2"]]

  :plugins [[lein-tailwind "0.1.2"]]

  :source-paths ["src"]

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "flaglib2.test-runner"]}

  :profiles {:dev {:dependencies [[com.bhauman/figwheel-main "0.2.16"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]
                                  [day8.re-frame/re-frame-10x "1.2.2"]]
                   :resource-paths ["target"]
                   ;; need to add the compiled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["target"]}}

  ;;Utterly ignorant so far
  :tailwind {:tailwind-dir "src/css/tailwind"
            :output-dir   "src/css"
            :tailwind-config  "tailwind.config.js" ;; tailwind.config.js is the default value 
            :styles [{:src "main.css"
                      :dst "main.css"}]})

