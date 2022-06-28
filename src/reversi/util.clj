(ns reversi.util)

(import
  '(java.awt Color))

(defn half
  [num]
  (float (/ num 2)))

(defn cartesian-product
  [m n]
  (vec (apply concat (map (fn [mi]
                            (mapv (fn [ni]
                                    [mi ni]) n)) m))))

(defn gen-grid
  [m n def-val]
  (vec (repeat m (vec (repeat n def-val)))))

(defn change-square
  [grid r c val]
  (assoc grid r (assoc (grid r) c val)))

(defn lookup
  [grid r c]
  ((grid r) c))