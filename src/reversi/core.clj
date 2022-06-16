(ns reversi.core
  (:use seesaw.core))

(defn -main [& args]
  (invoke-later
    (-> (frame :title "Hello",
           :content "Hello world from seesaw, a new gui library to avoid the horrors of swing!",
           :on-close :exit)
     pack!
     show!)))