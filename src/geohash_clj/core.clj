(ns geohash-clj.core)

(def base32-chars "0123456789bcdefghjkmnpqrstuvwxyz")
(def base32->int (zipmap base32-chars (range)))
(def int->base32 (zipmap (range) base32-chars))

(defn int->bit-col [n]
  (reduce #(cons (if (bit-test n %2) 1 0) %1) () (range 5)))


(defn geohash->bits [geohash]
  (when (every? #(base32->int %) geohash)
    (mapcat #(int->bit-col (base32->int %)) geohash)))


(defn separate [s]
  (let [cols (partition 2 s)]
    (list (map last cols) (map first cols))))


(defn locate [[min-loc max-loc] col]
  (let [mid ( / (+ min-loc max-loc) 2)]
    (if (empty? col)
      [(float min-loc) (float max-loc)]
      (recur (if (zero? (first col)) [min-loc mid] [mid max-loc]) (rest col)))))


(defn encode [lat lon])


(defn decode [geohash]
  (let [[lat lon] (separate (geohash->bits geohash))]
    [(locate [-90 90] lat) (locate [-180 180] lon)]))
