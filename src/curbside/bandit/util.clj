(ns curbside.bandit.util)

(def const-e 2.71828)

(defn select-by-probability
  "Given a map of keys where the values are probabilities (summing to one),
  randomly select a key with probability proportional to its assigned
  probability."
  [probs]
  (let [sorted-probs (sort-by val probs)
        cumulative-probs (reductions + (map val sorted-probs))
        rand-val (rand 1.0)
        chosen-index (->> cumulative-probs
                          (map (fn [i p] [i p]) (range))
                          (filter (fn [[i p]] (< rand-val p)))
                          first
                          first)]
    (first (nth sorted-probs chosen-index))))

(defn fmt-key
  "Safely interpolate values into a key format. Values to be interpolated can
  contain any character *except* ':', which is used to separate the parts of
  each key. fmt must be a format expected by clojure.core/format."
  [fmt & args]
  (let [xargs (map #(if (keyword? %) (name %) (str %)) args)]
    (assert (->> xargs
                 (not-any? (partial re-find #":")))
            (str "invalid characters found in argument to fmt-key " args))
    (apply format fmt xargs)))

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
