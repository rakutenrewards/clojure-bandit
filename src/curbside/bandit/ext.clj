(ns curbside.bandit.ext
  (:require
   [clojure.walk :as walk])
  (:import
   (clojure.lang MapEntry)))

(defn stringify-keys
  [m]
  (into {}
        (for [[k v] m]
          [(name k) v])))

(defn keywordify-keys
  ([m]
   (keywordify-keys nil m))
  ([namespace m]
   (into {}
         (for [[k v] m]
           [(keyword namespace k) v]))))

(defn contains-in?
  "true iff entire path of keys exists. For use with update-in."
  [m ks]
  (let [submaps (reductions get m ks)
        bools (map contains? submaps ks)]
    (every? identity bools)))

(defn update-in-if-contains
  "update-in if possible, otherwise return map unchanged."
  [m ks f]
  (if (contains-in? m ks)
    (update-in m ks f)
    m))

(defn parse-double
  [x]
  (Double/parseDouble x))

(defn parse-int
  [x]
  (Integer/parseInt x))

(defn pop-n
  "Pops n items from a persistent queue. Returns a persistent queue."
  [n q]
  (apply conj clojure.lang.PersistentQueue/EMPTY (drop n q)))

(defn map-kvs
  "Map a function that takes [key value] pairs and returns values
  over the given map. Keys are left unchanged."
  [f m]
  (into {} (map (fn [[x y]] [x (f x y)]) m)))

(defn recursive-map-kvs
  "Walks a structure, applying the user-supplied function f to all key-value
  pairs. Example:

  (recursive-map-kvs
    (fn [[k v]] (if (= k :bar) [k (+ 1 v)] [k v]))
    {:foo [{:bar 1} {:bar 2}] :bar 3 :baz 4})

  Returns:

  {:foo [{:bar 2} {:bar 3}] :bar 4 :baz 4}"
  [f m]
  (let [transform-kv (fn [x]
                       (if (= MapEntry (type x))
                         (f x)
                         x))]
    (walk/prewalk transform-kv m)))
