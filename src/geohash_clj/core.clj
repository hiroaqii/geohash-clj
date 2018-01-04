(ns geohash-clj.core)

(def base32 (zipmap "0123456789bcdefghjkmnpqrstuvwxyz" (range)))


(defn zero-pad [s]
  (let [n (- 5 (count s))
        col (map #(- (int %) 48) s)]
    (concat (repeat n 0) col)))


(defn geohash->bits [geohash]
  (when (every? #(base32 %) geohash)
    (mapcat
     #(->> (base32 %)
           (Integer/toBinaryString)
           (zero-pad))
     geohash)))


(defn separate [s]
  (let [cols (partition 2 s)]
    (list (map first cols) (map last cols))))


(defn locate [[min-loc max-loc] col]
  (let [mid ( / (+ min-loc max-loc) 2)]
    (if (empty? col)
      [(float min-loc) (float max-loc)]
      (recur (if (zero? (first col)) [min-loc mid] [mid max-loc]) (rest col)))))


(defn encode [lat lon])


(defn decode [geohash]
  (let [[lat lon] (separate (geohash->bits geohash))]
   [(locate [-180 180] lat) (locate [-90 90] lon)]))
