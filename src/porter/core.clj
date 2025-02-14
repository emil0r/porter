(ns porter.core
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [sci.core :as sci]))


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
                       ctx ctx
                       counter 30
                       idx 0]
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
                          (nil? v)
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
  (let [ctx'    (-> (reduce (fn [out path]
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

(defn get-presence [ctx ks]
  (seq (reduce (fn [out k]
                 (cond (and (keyword? k)
                            (not (get ctx k)))
                       (conj out k)

                       (and (vector? k)
                            (not (get-in ctx k)))
                       (conj out k)

                       :else
                       out))
               #{} ks)))


(defn build-output [env src {:keys [dest print namespaces injections exec]} ctx-paths]
  (let [ctx       (create-ctx (merge {:env env} injections) ctx-paths namespaces)
        ks-in-src (mapv (comp
                         (fn [[s exp :as v]]
                           (if (vector? exp)
                             [s (->> exp
                                     (map (fn [x]
                                            (if (vector? x)
                                              (get-in ctx x)
                                              x)))
                                     (vec))]
                             v))
                         (fn [[s exp]]
                           (try
                             [s (edn/read-string exp)]
                             (catch Exception e
                               (throw (ex-info "Unable to parse expression" {:expression exp
                                                                             :e          e}))))))
                        (re-seq #"\{\{([^\}]+)\}\}" src))
        diff      (get-presence ctx (map second ks-in-src))]
    (cond
      diff
      (println "Missing keys" {:diff      diff
                               :ctx-paths ctx-paths})

      :else
      (let [config (reduce (fn [out [s k]]
                             (let [v (str (or (get ctx k)
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
