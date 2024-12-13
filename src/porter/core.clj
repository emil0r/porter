(ns porter.core
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [sci.core :as sci]))


(defmulti branch? class)
(defmethod branch? :default [_] false)
(defmethod branch? clojure.lang.IPersistentVector [v] true)
(defmethod branch? clojure.lang.IPersistentMap [m] true)
(defmethod branch? clojure.lang.IPersistentList [l] true)
(defmethod branch? clojure.lang.ISeq [s] true)
(defmethod branch? clojure.lang.MapEntry [s] true)


(defmulti seq-children class)
(defmethod seq-children clojure.lang.IPersistentVector [v] v)
(defmethod seq-children clojure.lang.IPersistentMap [m] (mapv identity m))
(defmethod seq-children clojure.lang.IPersistentList [l] l)
(defmethod seq-children clojure.lang.ISeq [s] s)
(defmethod seq-children clojure.lang.MapEntry [me] me)

(defmulti make-node (fn [node children] (class node)))
(defmethod make-node clojure.lang.IPersistentVector [v children] (vec children))
(defmethod make-node clojure.lang.IPersistentMap [m children] (into {} children))
(defmethod make-node clojure.lang.IPersistentList [_ children] (apply list children))
(defmethod make-node clojure.lang.LazySeq [_ children] (apply list children))
(defmethod make-node clojure.lang.ISeq [node children] (apply list children))
(defmethod make-node clojure.lang.MapEntry [v children] (vec children))

(prefer-method make-node clojure.lang.IPersistentList clojure.lang.ISeq)
(prefer-method branch? clojure.lang.IPersistentList clojure.lang.ISeq)
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

(defn- massage-ctx [sci-ctx ctx]
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
                       counter 7
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
                                 (inc idx)))))))]
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
                 (merge ctx))
        sci-ctx (sci/init {:namespaces (merge
                                        (when-not (= 'user (ns-name *ns*))
                                          {'user {'ctx ctx'}})
                                        {(ns-name *ns*) (merge {'ctx ctx'}
                                                               (ns-publics *ns*))}
                                        (->> namespaces
                                             (map (fn [ns]
                                                    {ns (ns-publics ns)}))
                                             (into {})))})]
    (massage-ctx sci-ctx ctx')))

(defn re-k [k]
  (re-pattern (reduce (fn [out c]
                        (str/replace out (re-pattern (str "\\" c))
                                     (str "\\\\" c)))
                      (str "\\{\\{\\s{0,}("
                           (-> (str k)
                               (str/replace #"\[" "\\\\[")
                               (str/replace #"\]" "\\\\]"))
                           ")\\s{0,}\\}\\}") "+.")))

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
  (let [ctx (create-ctx (merge {:env env} injections) ctx-paths namespaces)
        ks-in-src (map (fn [[s exp]]
                         [s (edn/read-string exp)])
                       (re-seq #"\{\{([^\}]+)\}\}" src))
        diff (get-presence ctx (map second ks-in-src))]
    (cond
      diff
      (println "Missing keys" {:diff diff
                               :ctx-paths ctx-paths})

      :else
      (let [config (reduce (fn [out [s k]]
                             (let [v (str (or (get ctx k)
                                              (get-in ctx k)))]
                               (str/replace out (re-k k) v)))
                           src ks-in-src)]
        (when print
          (println config))
        (when dest
          (spit dest config))
        (when exec
          (shell config))
        config))))

(defn build-output-from-file [env src opts ctx-paths]
  (if (not (fs/exists? src))
    (println "src does not exist" {:src src})
    (build-output env (slurp src) opts ctx-paths)))
