(ns reversi.util)

(import
  '(java.awt Color))

(defn half
  [num]
  (float (/ num 2)))

(defn grid-value
  ([]
   (grid-value (. Color blue)))
  ([color]
   {:color color}))

(defn gen-grid
  [m n def-val]
  (vec (repeat m (vec (repeat n def-val)))))

(defn change-square
  [grid [r c] val]
  (assoc grid r (assoc (grid r) c val)))

(defn lookup
  [grid [r c]]
  ((grid r) c))

(defn index-value
  [grid]
  (let [nums (range (count grid))]
    (for [r nums
          c nums]
      [[r c] (lookup grid [r c])])))

(defn locations
  [grid predicate]
    (filter (fn [[loc v]]
              (predicate loc v)) (index-value grid)))

(defn color-locations
  [grid color]
  (locations grid (fn [_ v]
                    (= v (grid-value color)))))

(defn start-grid
  ([]
   (start-grid 8))
  ([dim]
  (-> (gen-grid dim dim {}) (change-square [3 3] (grid-value (. Color blue)))
      (change-square [3 4] (grid-value (. Color orange)))
      (change-square [4 3] (grid-value (. Color orange)))
      (change-square [4 4] (grid-value (. Color blue))))))