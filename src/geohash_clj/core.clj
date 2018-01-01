(ns geohash-clj.core)

(def base32 (zipmap "0123456789bcdefghjkmnpqrstuvwxyz" (range)))

(defn geohash->base10 [geohash]
  (when (every? #(base32 %) geohash)
    (->> geohash
         (map #(base32 %))
         (reduce #(bit-or (bit-shift-left %1 5) %2)))))

(defn encode [lat lon])

(defn decode [geohash])
