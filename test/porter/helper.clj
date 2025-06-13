(ns porter.helper)

(defn my-test-fn
  "This is referred to via SCI. Leave as is"
  [ctx]
  (format "Foobar is %s and Foobaz is %s"
          (str (get-in ctx [:test-fn-data :foobar]))
          (str (get-in ctx [:test-fn-data :foobaz]))))
