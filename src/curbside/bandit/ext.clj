(ns curbside.bandit.ext
  (:import
   (clojure.lang PersistentQueue)))

(defn stringify-keys
  "Converts the keys of the given map to strings."
  [m]
  (into {}
        (for [[k v] m]
          [(name k) v])))

(defn keywordify-keys
  "Converts the keys of the given map to keywords."
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
  (apply conj PersistentQueue/EMPTY (drop n q)))

(defn map-kvs
  "Map a function that takes [key value] pairs and returns values
  over the given map. Keys are left unchanged."
  [f m]
  (into {} (map (fn [[x y]] [x (f x y)]) m)))
