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
