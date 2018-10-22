(ns checkers.audio
  (:require [clojure.java.io :as io])
  (:import (javax.sound.sampled AudioSystem)
           (java.io File)))

(def sounds {:lost "sounds/game-lost.wav"
             :won "sounds/game-won.wav"
             :move "sounds/move.wav"
             :jump "sounds/jump.wav"})

(defn play-sound [sound]
  (if-let [clip (get sounds sound)]
    (.start
      (doto (AudioSystem/getClip)
        (.open
          (AudioSystem/getAudioInputStream
            ^File (io/as-file (io/resource clip))))))
    (println "Error reading file:" sound)))
