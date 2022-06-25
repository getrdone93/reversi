(ns reversi.util)

(defn half
  [num]
  (float (/ num 2)))

(defn cartesian-product
  [m n]
  (vec (apply concat (map (fn [mi]
                            (mapv (fn [ni]
                                    [mi ni]) n)) m))))
