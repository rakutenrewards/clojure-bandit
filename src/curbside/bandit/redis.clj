(ns curbside.bandit.redis
  "Redis utility functions.")

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

(defn ->redis-map
  "Convert the raw result of an `hgetall` command to a map."
  [x]
  (->> x
       (partition 2)
       (map #(into [] %))
       (into {})))
