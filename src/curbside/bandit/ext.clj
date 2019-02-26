(ns curbside.bandit.ext)

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
