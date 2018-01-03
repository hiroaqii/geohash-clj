(ns geohash-clj.core)

(def base32 (zipmap "0123456789bcdefghjkmnpqrstuvwxyz" (range)))

(defn geohash->base10 [geohash]
  (when (every? #(base32 %) geohash)
    (->> geohash
         (map #(base32 %))
         (reduce #(bit-or (bit-shift-left %1 5) %2)))))

(defn separate [n]
  (loop [x 0 y 0 i 0]
    (if (> i 60)
      [x y]
      (recur
       (if (bit-test n (inc i))(bit-set x (/ i 2)) x)
       (if (bit-test n i)(bit-set y (/ i 2)) y)
       (+ i 2)))))

(defn encode [lat lon])

(defn decode [geohash])
