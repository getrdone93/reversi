(ns reversi.core
  (:require [reversi.util :as util]))

(import
  '(java.awt Color Dimension)
  '(java.awt.image BufferedImage)
  '(javax.swing JPanel JFrame))

(def grid-dims {:dim 900 :num-sqs 8 :sq-dim 100})

(def frame-title "reversi")

(defn draw-circle
  ([im-graph sq-dim thick [sq-x sq-y]]
   (draw-circle im-graph (+ sq-dim thick) [sq-x sq-y]))
  ([im-graph tot-sq-wid [sq-x sq-y]]
   (let [hw (util/half tot-sq-wid)
         diam 75
         rad (util/half diam)
         st-pos (- rad 5)]
     (.setColor im-graph (. Color blue))

     (.fillOval im-graph (- (+ (* sq-x tot-sq-wid) hw) st-pos)
                (- (+ (* sq-y tot-sq-wid) hw) st-pos)
                diam diam)
     )))

(defn draw-grid
  "Draws a grid with setColor and fillRect calls."
  [im-graph bd draw-x draw-y border-thick sq-dim]
  (if (< draw-x bd)
    (do
      (.setColor im-graph (. Color gray))
      (.fillRect im-graph draw-x draw-y sq-dim sq-dim)
      (draw-grid im-graph bd (+ draw-x sq-dim border-thick) draw-y border-thick sq-dim))
    (when (< (+ draw-y border-thick) bd)
      (draw-grid im-graph bd border-thick (+ draw-y sq-dim border-thick) border-thick sq-dim))))


(defn color-frame
  "Invokes sub functions and draws the entire frame."
  [g {d :dim nsqs :num-sqs sq-dim :sq-dim}]
  (let [img (new BufferedImage d d (. BufferedImage TYPE_INT_ARGB))
        id (. img (getWidth))
        im-graph (. img (getGraphics))
        thick (Math/round (float (/ (- id (* nsqs sq-dim)) (inc nsqs))))
        bd (- id (* 2 thick))]
    (.setColor im-graph (. Color black))
    (.fillRect im-graph 0 0 id id)
    (draw-grid im-graph bd thick thick thick sq-dim)
    (doall
      (for [x (range nsqs)
          y (range nsqs)]
      (draw-circle im-graph sq-dim thick [x y])))
    (. g (drawImage img 0 0 nil))
    (. im-graph (dispose))))

(defn panel
  "Returns a new JPanel object with its paint function overriden
   by color-frame."
  []
  (let [dim (grid-dims :dim)]
    (doto (proxy [JPanel] []
            (paint [g] (color-frame g grid-dims)))
      (.setPreferredSize (new Dimension dim dim)))))

(defn frame
  "Draws a new frame."
  [bv frame-title]
  (doto
    (new JFrame)
    (.setTitle (str frame-title))
    (-> (.getContentPane) (.add (panel)))
    .pack
    .show))

(defn -main [& args]
  (frame 10 frame-title))