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
      (float mid)
      (recur (if (zero? (first col)) [min-loc mid] [mid max-loc]) (rest col)))))


(defn decode [geohash]
  (let [[lat lon] (separate (geohash->bits geohash))]
    [(locate [-90 90] lat) (locate [-180 180] lon)]))


(defn int-loc->bit-col-loc [loc min-loc max-loc size]
  (loop [min-loc min-loc
         max-loc max-loc
         size size
         ret []]
    (let [mid-loc (/ (+ min-loc max-loc) 2)]
      (if (zero? size)
        ret
        (if (< mid-loc loc)
          (recur mid-loc max-loc (dec size) (conj ret 1))
          (recur min-loc mid-loc (dec size) (conj ret 0)))))))


(defn merge-col [lat-bits lon-bits]
  (let [col (interleave lon-bits lat-bits)]
    (if (= (count lat-bits) (count lon-bits))
      col
      (concat col [(last lon-bits)]))))


(defn bit-col->int [col]
  (loop [col col
         n (dec (count col))
         ret 0]
    (if (empty? col)
      ret
      (recur (rest col) (dec n) (if (zero? (first col)) ret (bit-flip ret n))))))


(defn encode [lat lon size]
  (let [size (min size 12)
        lat-size (quot (* 5 size) 2)
        lon-size (if (even? size) lat-size (inc lat-size))
        lat-bit-col (int-loc->bit-col-loc lat -90 90 lat-size)
        lon-bit-col (int-loc->bit-col-loc lon -180 180 lon-size)]
    (->> (merge-col lat-bit-col lon-bit-col)
         (partition 5)
         (map bit-col->int)
         (map int->base32)
         (apply str))))
