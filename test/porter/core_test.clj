(ns porter.core-test
  (:require [clojure.string :as str]
            [porter.core :as sut]
            [porter.helper]
            [clojure.test :as t]))


(t/deftest basic-usage-test
  (let [output (sut/build-output :test (slurp "dev-resources/test.yml")
                                 nil ["dev-resources/test.base.edn"])]
    (t/is (true? (and (str/includes? output "full-path-here")
                      (str/includes? output "another-full-path-here"))))))

(t/deftest missing-keys-test
  (t/is
   (str/starts-with?
    (with-out-str
      (sut/build-output :test (slurp "dev-resources/test.yml")
                        nil []))
    "Invalid input")))

(t/deftest functions-usage-test
  (let [output (sut/build-output :test "{{:test-data}}"
                                 {:namespaces ['porter.helper]}
                                 ["dev-resources/test.functions.edn"])]
    (t/is (= output
             "Foobar is true and Foobaz is false"))))

(t/deftest keys-are-offset-test
  (let [output (sut/build-output :test "{{ :full.path/here}}"
                                 {:namespaces ['porter.helper]}
                                 ["dev-resources/test.base.edn"])]
    (t/is (= output
             "full-path-here")))

  (let [output (sut/build-output :test "{{ [:full.path/here   ]}}"
                                 {:namespaces ['porter.helper]}
                                 ["dev-resources/test.base.edn"])]
    (t/is (= output
             "full-path-here")))

  (let [output (sut/build-output :test "{{[:full.path/here] }}"
                                 {:namespaces ['porter.helper]}
                                 ["dev-resources/test.base.edn"])]
    (t/is (= output
             "full-path-here")))
  (let [output (sut/build-output :test "{{ [:full.path/here]}}"
                                 {:namespaces ['porter.helper]}
                                 ["dev-resources/test.base.edn"])]
    (t/is (= output
             "full-path-here"))))

(t/deftest validity-test
  (let [output (sut/check-validity :test (slurp "dev-resources/test.validity.yml")
                                   nil ["dev-resources/test.ctx.validity.edn"])]
    (t/is (= #{:ctx3 [:ctx4 :env] [:empty-path]}
             (:empty-paths output))
          "missing paths")
    (t/is (= #{"{{ [:ctx2 :name }}"}
             (:broken-paths output))
          "invalid paths")))
