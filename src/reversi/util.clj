(ns reversi.util)

(import
  '(java.awt Color))

(defn half
  [num]
  (float (/ num 2)))

(defn grid-value
  ;([]
  ; (grid-value (. Color blue)))
  ([color]
   {:color color}))

(defn gen-grid
  [m n def-val]
  (vec (repeat m (vec (repeat n def-val)))))

(defn gen-grid-pos
  [m n]
  (mapv vec (partition n (for [r (range m)
                               c (range n)]
                           {:pos [r c]}))))

;(defn change-square
;  [grid [r c] val]
;  (assoc grid r (assoc (grid r) c val)))

(defn change-square
  [grid [r c] val]
  (assoc grid r (assoc (grid r) c (merge ((grid r) c) val))))

(defn dim-check
  [dim [sr sc :as rc]]
  (if (and (< -1 sr) (< sr dim)
           (< -1 sc) (< sc dim))
    rc))

(defn lookup
  [grid [r c]]
  (if (dim-check (count grid) [r c])
    ((grid r) c)))

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

(defn move-right
  [[sr sc]]
  [sr (inc sc)])

(defn reverse-pieces
  ([grid start-pos oppo-col mov-func]
   (reverse-pieces grid (mov-func start-pos) oppo-col mov-func []))
  ([grid pos oppo-col mov-func res]
   (let [{piece-col :color :as piece} (lookup grid pos)]
     (cond
       (nil? piece) res
       (or (empty? piece)
           (not= piece-col oppo-col)) (conj res piece)
       :else (recur grid (mov-func pos) oppo-col mov-func (conj res piece))))))

(defn reversi?
  ([oppo-col pieces]
   (reversi? oppo-col pieces {} :color))
  ([oppo-col pieces empty col-key]
   (and (= empty (last pieces))
        (= (set (drop-last pieces)) #{{col-key oppo-col}}))))

(defn start-grid
  ([]
   (start-grid 8))
  ([dim]
  (-> (gen-grid dim dim {}) (change-square [3 3] (grid-value (. Color blue)))
      (change-square [3 4] (grid-value (. Color orange)))
      (change-square [4 3] (grid-value (. Color orange)))
      (change-square [4 4] (grid-value (. Color blue))))))


(defn test-grid-oppo
  ([]
   (test-grid-oppo 8))
  ([dim]
   (-> (gen-grid-pos dim dim) (change-square [3 3] (grid-value (. Color blue)))
       (change-square [3 4] (grid-value (. Color orange)))
       (change-square [3 5] (grid-value (. Color orange)))
       (change-square [3 6] (grid-value (. Color blue)))

       (change-square [4 3] (grid-value (. Color orange)))
       (change-square [4 4] (grid-value (. Color blue))))))

(defn test-grid-empty
  ([]
   (test-grid-empty 8))
  ([dim]
   (-> (gen-grid dim dim {}) (change-square [3 3] (grid-value (. Color blue)))
       (change-square [3 4] (grid-value (. Color orange)))
       (change-square [3 5] (grid-value (. Color orange)))

       (change-square [4 3] (grid-value (. Color orange)))
       (change-square [4 4] (grid-value (. Color blue))))))


(defn test-grid-no-move
  ([]
   (test-grid-no-move 8))
  ([dim]
   (-> (gen-grid dim dim {}) (change-square [3 3] (grid-value (. Color blue)))
       (change-square [3 4] (grid-value (. Color blue)))

       (change-square [4 3] (grid-value (. Color orange)))
       (change-square [4 4] (grid-value (. Color blue))))))

(defn test-grid-no-move-empty
  ([]
   (test-grid-no-move-empty 8))
  ([dim]
   (-> (gen-grid dim dim {}) (change-square [3 3] (grid-value (. Color blue)))

       (change-square [4 3] (grid-value (. Color orange)))
       (change-square [4 4] (grid-value (. Color blue))))))