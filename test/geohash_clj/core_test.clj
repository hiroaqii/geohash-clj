(ns geohash-clj.core-test
  (:require [clojure.test :refer :all]
            [geohash-clj.core :refer :all]))

(deftest test-base32->int
  (is (= 0 (base32->int \0)))
  (is (= 31 (base32->int \z)))
  (is (every? nil? (map #(base32->int %) "ailo"))))


(deftest test-int->bit-col
  (is (= [0 0 0 0 0] (int->bit-col 0)))
  (is (= [0 0 0 0 1] (int->bit-col 1)))
  (is (= [0 0 0 1 1] (int->bit-col 3)))
  (is (= [0 1 0 0 0] (int->bit-col 8)))
  (is (= [1 1 1 1 1] (int->bit-col 31))))


(deftest test-geohash->bits
  (is (= [0 0 0 0 0] (geohash->bits "0")))
  (is (= [0 1 0 1 0] (geohash->bits "b")))
  (is (= [1 1 1 1 1] (geohash->bits "z")))
  (is (= [1 1 1 1 1 0 1 0 1 0 0 0 0 0 0](geohash->bits "zb0")))
  (is (nil? (geohash->bits "za0"))))


(deftest test-separate
  (is (= [[2 4 6 8] [1 3 5 7]] (separate [1 2 3 4 5 6 7 8]))))


(deftest test-locate
  (is (= [-90.0 0.0   ](locate [-90 90] [0])))
  (is (= [0.0 90.0    ](locate [-90 90] [1])))
  (is (= [0.0 45.0    ](locate [-90 90] [1 0])))
  (is (= [45.0 90.0   ](locate [-90 90] [1 1])))
  (is (= [67.5 90.0   ](locate [-90 90] [1 1 1])))
  (is (= [78.75 90.0  ](locate [-90 90] [1 1 1 1])))
  (is (= [78.75 84.375](locate [-90 90] [1 1 1 1 0]))))

(deftest test-int-loc->bit-col-loc
  (is (= (int-loc->bit-col-loc 42.583 -90 90 10) [1 0 1 1 1 1 0 0 1 0])))

(deftest test-bit-col->int
  (is (= (bit-col->int [0 0 0 0 0]) 0))
  (is (= (bit-col->int [0 0 1 0 0]) 4))
  (is (= (bit-col->int [0 1 0 0 1]) 9))
  (is (= (bit-col->int [1 0 0 0 0]) 16))
  (is (= (bit-col->int [1 1 1 1 1]) 31)))


(deftest test-merge-col
  (is (= (merge-col [1 2 3] [4 5 6]) [4 1 5 2 6 3]))
  (is (= (merge-col [1 2 3] [4 5 6 7]) [4 1 5 2 6 3 7])))

;https://en.wikipedia.org/wiki/Geohash
(deftest test-encode
  (is (= (encode 57.64911,10.40744 1) "u"))
  (is (= (encode 57.64911,10.40744 2) "u4"))
  (is (= (encode 57.64911,10.40744 3) "u4pr"))
  (is (= (encode 57.64911,10.40744 4) "u4pr"))
  (is (= (encode 57.64911,10.40744 5) "u4pru"))
  (is (= (encode 57.64911,10.40744 6) "u4pruy"))
  (is (= (encode 57.64911,10.40744 7) "u4pruyd"))
  (is (= (encode 57.64911,10.40744 8) "u4pruydq"))
  (is (= (encode 57.64911,10.40744 9) "u4pruydqq"))
  (is (= (encode 57.64911,10.40744 10) "u4pruydqqv"))
  (is (= (encode 57.64911,10.40744 11) "u4pruydqqvj")))
