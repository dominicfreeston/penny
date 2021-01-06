;; copied and modified from https://github.com/trystan/random-seed

(ns penny.random-seed
  (:require [quil.core :as q])
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle]))

(defonce rng (new java.util.Random))

(defn- seed-from-timestamp
  "Generates a suitable random seed from a string obtained by calling timestamp"
  [timestamp]
  (-> (str timestamp "1")
      (clojure.string/replace #"-" "")
      clojure.string/reverse
      Long/parseLong))

(defn set-random-seed!
  "Sets the seed of the Clojure random function replacements as well as the quil random and noise seeds."
  [seed]
  (q/random-seed seed)
  (q/noise-seed seed)
  (.setSeed rng (long seed)))

(defn set-timestamp-as-random-seed!
  "Sets the seed using a value obtained from penny.utils/timestamp"
  [timestamp]
  (set-random-seed! (seed-from-timestamp timestamp)))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive). Works like clojure.core/rand except it
  uses the seed specified in set-random-seed!."
  ([] (.nextFloat rng))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  Works like clojure.core/rand except it uses the seed specified in
  set-random-seed!."
  [n]
  (int (rand n)))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection. Works like clojure.core/rand except it uses the seed
  specified in set-random-seed!."
  [coll]
  (nth coll (rand-int (count coll))))

(defn shuffle
  "Return a random permutation of coll. Works like clojure.core/shuffle
  except it uses the seed specified in set-random-seed!."
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rng)
    (clojure.lang.RT/vector (.toArray al))))
