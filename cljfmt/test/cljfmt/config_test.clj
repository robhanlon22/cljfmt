(ns cljfmt.config-test
  (:require [cljfmt.config :as config]
            [cljfmt.main :as main]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [are deftest is testing]]))

(deftest test-validate-config
  (testing "rejects EDN quoted symbols"
    (let [edn-key (ffirst (edn/read-string "{'let #{0}}"))]
      (is (not (s/valid? ::config/config {:blank-line-forms {edn-key #{0}}})))))
  (testing "rejects CLJ quoted forms"
    (let [clj-key (-> (read-string "{'let #{0}}") ffirst)]
      (is (not (s/valid? ::config/config {:blank-line-forms {clj-key #{0}}})))))
  (testing "valid bare symbols are accepted"
    (is (s/valid? ::config/config {:blank-line-forms {'cond :all 'let #{0}}})))
  (testing "valid keys pass through for all symbol-keyed config maps"
    (is (s/valid? ::config/config {:extra-blank-line-forms {'cond :all}}))
    (is (s/valid? ::config/config {:aligned-forms {'let #{0}}}))
    (is (s/valid? ::config/config {:extra-aligned-forms {'let #{0}}}))
    (is (s/valid? ::config/config {:indents {'defn [[:inner 0]]}}))
    (is (s/valid? ::config/config {:extra-indents {'defn [[:inner 0]]}})))
  (testing "valid regex pattern keys are accepted"
    (is (s/valid? ::config/config {:indents {#"^def" [[:inner 0]]}}))
    (is (s/valid? ::config/config {:indents {[#"clojure.core" #"^def"] [[:inner 0]]}})))
  (testing "handles nil and missing values"
    (is (s/valid? ::config/config {}))
    (is (not (s/valid? ::config/config nil)))))

(deftest test-validate-alignment-rules
  (is (s/valid? ::config/config
                {:aligned-forms {'let #{0} 'cond :all 'case [:block 1]}
                 :extra-aligned-forms {'with-bindings [0 2] 'binding nil}}))
  (are [rule] (not (s/valid? ::config/config {:aligned-forms {'example rule}}))
    #{-1}
    [:block]
    [:block -1]
    [:block 1 2]
    [:unknown 1]))

(deftest test-load-invalid-alignment-rules
  (let [file (java.io.File/createTempFile "cljfmt-config" ".edn")]
    (try
      (doseq [option [:aligned-forms :extra-aligned-forms]]
        (spit file (pr-str {option {'case [:block -1]}}))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Spec assertion failed"
                              (config/load-config (.getPath file)))))
      (finally
        (.delete file)))))

(deftest test-reader-conditional-cli-option
  (let [source-file (java.io.File/createTempFile "cljfmt-source" ".cljc")
        config-file (java.io.File/createTempFile "cljfmt-config" ".edn")
        source "#?(:clj 1\n   :cljs 2)\n"
        aligned "#?(:clj  1\n   :cljs 2)\n"]
    (try
      (doseq [[configured? flag expected]
              [[false "--align-reader-conditionals" aligned]
               [true "--no-align-reader-conditionals" source]]]
        (spit config-file (pr-str {:align-reader-conditionals? configured?}))
        (spit source-file source)
        (main/-main "fix" (.getPath source-file)
                    "--config" (.getPath config-file) "--quiet" flag)
        (is (= expected (slurp source-file))))
      (finally
        (.delete source-file)
        (.delete config-file)))))

(deftest test-convert-legacy-keys
  (is (= {:indents {'foo [[:inner 0]]}}
         (config/convert-legacy-keys {:indents {'foo [[:inner 0]]}})))
  (is (= {:extra-indents {'foo [[:inner 0]]}}
         (config/convert-legacy-keys {:legacy/merge-indents? true
                                      :indents {'foo [[:inner 0]]}}))))

(deftest read-config-test
  (testing "Ensure that reading .clj config files works for valid data"
    (let [temp-file (java.io.File/createTempFile "test" ".clj")]
      (spit temp-file "{:key \"value\"}")
      (is (= {:key "value"} (config/read-config temp-file)))
      (.delete temp-file)))

  (testing "Ensure that reading .edn config files works"
    (let [temp-file (java.io.File/createTempFile "test" ".edn")]
      (spit temp-file "{:key \"value\"}")
      (is (= {:key "value"} (config/read-config temp-file)))
      (.delete temp-file))))

(deftest load-config-test
  (let [temp-dir (java.io.File/createTempFile "test" "")
        _ (do (.delete temp-dir)
              (.mkdir temp-dir))
        path (.getPath temp-dir)
        clj-file (java.io.File. temp-dir ".cljfmt.clj")
        delete-regex-ks #(dissoc % :file-pattern :indents)
        config (delete-regex-ks config/default-config)
        new-config (-> config
                       (update :remove-consecutive-blank-lines? not)
                       delete-regex-ks)
        config-with-read-clj (assoc new-config :read-clj-config-files? true)
        load-config #(delete-regex-ks (config/load-config %1 %2))]
    (testing "load-config ignores .clj config files when flag is false 
              (default)"
      (spit clj-file (pr-str new-config))
      (is (= (load-config path config) config)))

    (testing "load-config loads .clj config files when flag is true"
      (spit clj-file (pr-str config-with-read-clj))
      (is (= (load-config path config-with-read-clj)
             config-with-read-clj)))

    (testing "load-config always loads .edn config files"
      (let [edn-file (java.io.File. temp-dir ".cljfmt.edn")]
        (spit edn-file (pr-str new-config))
        (is (= (load-config path new-config)
               new-config))))

    (.delete temp-dir)))
