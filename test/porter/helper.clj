(ns porter.helper)

(defn my-test-fn [ctx]
  (format "Foobar is %s and Foobaz is %s"
          (str (get-in ctx [:test-fn-data :foobar]))
          (str (get-in ctx [:test-fn-data :foobaz]))))
