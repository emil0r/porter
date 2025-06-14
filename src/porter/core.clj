(ns porter.core
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [sci.core :as sci]))


(defn- escape-code
  [i]
  (str "\033[" i "m"))

(def colors-reset (escape-code 0))
(def colors-fg {:red   (escape-code 31)
                :white (escape-code 37)})
(def colors-bg {:on-red (escape-code 41)
                :on-white (escape-code 47)})


(defmulti branch? class)
(defmethod branch? :default                       [_] false)
(defmethod branch? clojure.lang.IPersistentVector [_] true)
(defmethod branch? clojure.lang.IPersistentMap    [_] true)
(defmethod branch? clojure.lang.IPersistentList   [_] true)
(defmethod branch? clojure.lang.ISeq              [_] true)
(defmethod branch? clojure.lang.MapEntry          [_] true)


(defmulti seq-children class)
(defmethod seq-children clojure.lang.IPersistentVector [v]  v)
(defmethod seq-children clojure.lang.IPersistentMap    [m]  (mapv identity m))
(defmethod seq-children clojure.lang.IPersistentList   [l]  l)
(defmethod seq-children clojure.lang.ISeq              [s]  s)
(defmethod seq-children clojure.lang.MapEntry          [me] me)

(defmulti make-node (fn [node _children] (class node)))
(defmethod make-node clojure.lang.IPersistentVector [_v children]    (vec children))
(defmethod make-node clojure.lang.IPersistentMap    [_m children]    (into {} children))
(defmethod make-node clojure.lang.IPersistentList   [_ children]     (apply list children))
(defmethod make-node clojure.lang.LazySeq           [_ children]     (apply list children))
(defmethod make-node clojure.lang.ISeq              [_node children] (apply list children))
(defmethod make-node clojure.lang.MapEntry          [_v children]    (vec children))

(prefer-method make-node    clojure.lang.IPersistentList clojure.lang.ISeq)
(prefer-method branch?      clojure.lang.IPersistentList clojure.lang.ISeq)
(prefer-method seq-children clojure.lang.IPersistentList clojure.lang.ISeq)

(defn zipper [node]
  (zip/zipper branch? seq-children make-node node))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      v)))

(defn- massage-ctx [namespaces ctx]
  (let [f (fn [ctx x]
            (cond
              (and (vector? x)
                   (not (map-entry? x))
                   (every? keyword? x))
              (get-in ctx x)

              :else
              x))
        pre-ctx (loop [next-loc (zipper ctx)
                       ctx      ctx
                       counter  30
                       idx      0]
                  (let [node (zip/node next-loc)]
                    (cond
                      (>= idx 100000)
                      (do (println "hit max limit" idx)
                          (zip/root next-loc))

                      (and (zip/end? next-loc)
                           (zero? counter))
                      (zip/root next-loc)

                      (zip/end? next-loc)
                      (recur (zipper (zip/root next-loc))
                             (zip/root next-loc)
                             (dec counter)
                             (inc idx))

                      :else
                      (let [v (f ctx node)]
                        (cond
                          (true? (:skip (meta node)))
                          (recur (zip/next next-loc)
                                 ctx
                                 counter
                                 (inc idx))

                          ;; value is nil and it's not a lookup
                          (and (nil? v)
                               (not (vector? node)))
                          (recur (zip/next next-loc)
                                 ctx
                                 counter
                                 (inc idx))

                          (not= v node)
                          (recur (zip/next (zip/replace next-loc v))
                                 ctx
                                 counter
                                 (inc idx))

                          :else
                          (recur (zip/next next-loc)
                                 ctx
                                 counter
                                 (inc idx)))))))
        sci-ctx (sci/init {:namespaces (merge
                                        (when-not (= 'user (ns-name *ns*))
                                          {'user {'ctx pre-ctx}})
                                        {(ns-name *ns*) (merge {'ctx pre-ctx}
                                                               (ns-publics *ns*))}
                                        (->> namespaces
                                             (map (fn [ns]
                                                    {ns (ns-publics ns)}))
                                             (into {})))})]
    (loop [next-loc (zipper pre-ctx)]
      (let [node (zip/node next-loc)]
        (cond
          (zip/end? next-loc)
          (zip/root next-loc)

          (list? node)
          (let [next (->> (sci/eval-form sci-ctx node)
                          (zip/replace next-loc)
                          (zip/next))]
            (recur next))

          :else (recur (zip/next next-loc)))))))

(defn create-ctx [ctx paths & [namespaces]]
  (assert "One of the paths does not exist" (every? fs/exists? paths))
  (let [ctx' (-> (reduce (fn [out path]
                           (deep-merge out (edn/read-string (slurp path))))
                         {} paths)
                 (merge ctx))]
    (massage-ctx namespaces ctx')))

(defn re-s [s]
  (re-pattern (-> s
                  (str/replace #"\[" "\\\\[")
                  (str/replace #"\]" "\\\\]")
                  (str/replace #"\{" "\\\\{")
                  (str/replace #"\}" "\\\\}"))))

(defn get-missing-presence [ctx ks]
  (seq (reduce (fn [out k]
                 (cond (and (keyword? k)
                            (nil? (get ctx k)))
                       (conj out k)

                       (and (vector? k)
                            (nil? (get-in ctx k)))
                       (conj out k)

                       :else
                       out))
               #{} ks)))

(defn get-ks-in-src [src ctx]
  (mapv (comp
         (fn [[s exp :as v]]
           (if (vector? exp)
             [s (->> exp
                     (map (fn [x]
                            (if (vector? x)
                              (get-in ctx x)
                              x)))
                     (vec))]
             v))
         (fn [[s expr]]
           (try
             [s (edn/read-string expr)]
             (catch Exception _
               [s ::invalid-key]))))
        (re-seq #"\{\{([^\}]+)\}\}" src)))

(defn invalid-key? [k]
  (= k ::invalid-key))


(defn check-validity
  ([ctx src]
   (let [ks-in-src    (get-ks-in-src src ctx)
         valid-ks     (->> ks-in-src
                           (remove (comp invalid-key? second))
                           (map second))
         invalid-ks   (->> ks-in-src
                           (filter (comp invalid-key? second))
                           (map first))
         empty-paths  (set (get-missing-presence ctx valid-ks))
         broken-paths (set invalid-ks)]
     {:empty-paths  empty-paths
      :broken-paths broken-paths}))
  ([env src {:keys [namespaces injections]} ctx-paths]
   (let [ctx (create-ctx (merge {:env env} injections) ctx-paths namespaces)]
     (check-validity ctx src))))


(defn build-output [env src {:keys [dest print namespaces injections exec]} ctx-paths]
  (let [ctx       (create-ctx (merge {:env env} injections) ctx-paths namespaces)
        ks-in-src (get-ks-in-src src ctx)
        validity  (check-validity ctx src)      ]
    (cond
      (or (seq (:broken-paths  validity))
          (seq (:empty-paths   validity)))
      (do (println "Invalid input")
          (when (seq (:broken-paths validity))
            (println "Broken paths" (:white colors-fg) (:on-red colors-bg) (:broken-paths validity) colors-reset))
          (when (seq (:empty-paths validity))
            (println "Empty paths" (:white colors-fg) (:on-red colors-bg) (:empty-paths validity) colors-reset)))

      :else
      (let [config (reduce (fn [out [s k]]
                             (let [v (str (if (some? (get ctx k))
                                            (get ctx k)
                                            (get-in ctx k)))]
                               (str/replace out (re-s s) v)))
                           src ks-in-src)]
        (when print
          (println config))
        (when dest
          (spit dest config))
        (when exec
          (doseq [to-exec (str/split config #"\n")]
            (when-not (str/blank? to-exec)
              (shell to-exec))))
        config))))

(defn build-output-from-file [env src opts ctx-paths]
  (if (not (fs/exists? src))
    (println "src does not exist" {:src src})
    (build-output env (slurp src) opts ctx-paths)))
