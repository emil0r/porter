(ns porter.core-test
  (:require [clojure.string :as str]
            [porter.core :as sut]
            [porter.helper]
            [clojure.test :as t]))


(t/deftest basic-usage
  (let [output (sut/build-output :test (slurp "dev-resources/test.yml")
                                 nil ["dev-resources/test.base.edn"])]
    (t/is (true? (and (str/includes? output "full-path-here")
                      (str/includes? output "another-full-path-here"))))))

(t/deftest missing-keys
  (t/is
   (str/starts-with?
    (with-out-str
      (sut/build-output :test (slurp "dev-resources/test.yml")
                        nil []))
    "Missing keys")))

(t/deftest functions-usage
  (let [output (sut/build-output :test "{{:test-data}}"
                                 {:namespaces ['porter.helper]}
                                 ["dev-resources/test.functions.edn"])]
    (t/is (= output
             "Foobar is true and Foobaz is false"))))

(t/deftest keys-are-offset
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
