(ns overtone-maschine.core
  (:require [overtone.live]))

(defn init-vector [length initializer]
  (loop [i 0 v (transient [])]
    (if (< i length)
      (recur (inc i) (conj! v (initializer i)))
      (persistent! v))))

(def audio-files (vector
  "/Users/Shared/Maschine Library/Samples/One Shots/Strike/Bend ClickStart.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Glitch/Crup Swag.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Acoustic Note/Purehigh D6.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Metal/Metallic Digigong 1.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Noise/Noise ProgHouse.wav"
  "/Users/Shared/Maschine Library/Samples/Instruments/Bass/Cute Monster Samples/CuteMonster f#3.wav"
  "/Users/Shared/Maschine Library/Samples/Instruments/Pad/Dweller Samples/Dweller c5.wav"
  "/Users/Shared/Maschine Library/Samples/Instruments/Mallet/Sansa Samples/Sansa C3.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Distortion/Dist BitBreakup.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Distortion/Dist CircuitBent 1.wav"))

(def maschineDials (list 11 12 13 14 15 16 17 18))
(def rolandDials   (list 4  5  6  7  8  9  10 11))
(def dialIDs maschineDials)

(def dials (init-vector (count audio-files) (fn [i]
  (list
    (list "pitch" "volume" "pace" "duration" "attack" "decay" "sustain" "release") ;; parameter
    (list  7       7        7      6          6        6       6         6)        ;; channel
    dialIDs                                                                        ;; dial ID
    (list  60      100      64     64         64       64      64        64)))))   ;; value

(overtone.live/on-event [:midi :control-change]
  (fn [{controller-number :note velocity :data1 data :velocity}]
    (println controller-number velocity data)
  ) ::control-handler)

(overtone.live/on-event [:midi :note-on]
  (fn [m]
    (let [note (:note m)]
      (println (overtone.live/midi->hz note) (:velocity-f m)))) ::note-handler)

(defn -main [] ())
