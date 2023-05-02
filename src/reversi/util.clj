(ns reversi.util)

(import
  '(java.awt Color))

(require '[clojure.set :as set])

(defn half
  "Divides a number in half and returns it as a float."
  [num]
  (/ num 2.0))

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

(defn move-down
  [[sr sc]]
  [(inc sr) sc])

(defn move-up
  [[sr sc]]
  [(dec sr) sc])

(defn move-left
  [[sr sc]]
  [sr (dec sc)])

(defn move-down-right
  [start-pos]
  (-> start-pos move-down move-right))

(defn move-down-left
  [start-pos]
  (-> start-pos move-down move-left))

(defn move-up-left
  [start-pos]
  (-> start-pos move-up move-left))

(defn move-up-right
  [start-pos]
  (-> start-pos move-up move-right))

(defn empty-tile?
  [tile]
  (= #{:pos} (set (keys tile))))

(defn remove-key
  ([tiles]
   (remove-key tiles :pos))
  ([tiles key]
  (mapv (fn [m]
          (dissoc m key)) tiles)))

(defn lookup-reverse-tiles
  ([grid start-pos oppo-col mov-func]
   (lookup-reverse-tiles grid (mov-func start-pos) oppo-col mov-func []))
  ([grid pos oppo-col mov-func res]
   (let [{piece-col :color :as tile} (lookup grid pos)]
     (cond
       (nil? tile) res
       (or (empty-tile? tile)
           (not= piece-col oppo-col)) (conj res tile)
       :else (recur grid (mov-func pos) oppo-col mov-func (conj res tile))))))

(defn reverse-tiles
  [move-pieces flip-color]
  (mapv (fn [tile]
       (merge tile {:color flip-color})) move-pieces))

(defn update-grid
  [grid updates]
  (reduce (fn [agg {[r c] :pos :as upd-tile}]
       (let [row (grid r)
             grid-tile (row c)]
         (assoc agg r (assoc row c (merge grid-tile upd-tile)))))
        grid updates))

(defn reversi?
  ([oppo-col pieces]
   (reversi? oppo-col pieces :color))
  ([oppo-col tiles col-key]
   (and (empty-tile? (last tiles))
        (= (set (remove-key (drop-last tiles)))
           #{{col-key oppo-col}}))))

(def piece-colors #{(. Color orange) (. Color blue)})

(defn opposite-color
  ([color]
   (opposite-color color piece-colors))
  ([color all-colors]
   (first (set/difference all-colors #{color}))))

(defn filter-grid
  [grid key value]
  (filterv (fn [tile]
               (when (= value (tile key))
                 tile)) (flatten grid)))

(defn moves
  ([grid curr-color]
   (into {} (map (fn [{p :pos}]
                   [p (moves grid p (opposite-color curr-color))])
                 (filter-grid grid :color curr-color))))
  ([grid start-pos oppo-col]
  (into {} (filterv some? (map (fn [[k f]]
            (let [pieces (lookup-reverse-tiles grid start-pos oppo-col f)]
              (if (reversi? oppo-col pieces)
                [k pieces])))
          {:right move-right :down-right move-down-right
           :down move-down :down-left move-down-left
           :left move-left :up-left move-up-left
           :up move-up :up-right move-up-right})))))

(defn random-choice
  [moves]
  (let [start-pos (rand-nth (keys moves))]
    [start-pos (rand-nth (into [] (moves start-pos)))]))

(defn player-move
  [board curr-color]
  (reverse-tiles (-> (random-choice (moves board curr-color))
                                        second second)
                                    curr-color))

(defn player-move-board
  [board curr-color]
  (update-grid board (player-move board curr-color)))

(defn game-loop
  [board curr-color]
  (recur (player-move-board board curr-color) (opposite-color curr-color)))

(defn start-grid
  ([]
   (start-grid 8))
  ([dim]
  (-> (gen-grid-pos dim dim) (change-square [3 3] (grid-value (. Color blue)))
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
   (-> (gen-grid-pos dim dim) (change-square [3 3] (grid-value (. Color blue)))
       (change-square [3 4] (grid-value (. Color orange)))
       (change-square [3 5] (grid-value (. Color orange)))

       (change-square [4 3] (grid-value (. Color orange)))
       (change-square [4 4] (grid-value (. Color blue))))))


(defn test-grid-no-move
  ([]
   (test-grid-no-move 8))
  ([dim]
   (-> (gen-grid-pos dim dim) (change-square [3 3] (grid-value (. Color blue)))
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