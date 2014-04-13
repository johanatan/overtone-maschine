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

(def maschineDials (vector 11 12 13 14 15 16 17 18))
(def rolandDials   (vector 4  5  6  7  8  9  10 11))
(def dialIDs maschineDials)

(def dials (init-vector (count audio-files) (fn [i]
  (vector
    (vector       "pitch" "volume" "pace" "duration" "attack" "decay" "sustain" "release") ;; parameter
    (vector       7       7        7      6          6        6       6         6)         ;; channel
    dialIDs                                                                                ;; dial ID
    (atom (vector 60      100      64     64         64       64      64        64))))))   ;; value

(defn get-dial-ids [dial-set]
  (nth dial-set 2))

(defn get-values [dial-set]
  (nth dial-set 3))

(defn get-dial-names [dial-set]
  (nth dial-set 0))

(defn get-dial-val [dial-set dial]
  (nth (get-values dial-set) (.indexOf (get-dial-names dial-set) dial)))

(defn get-param-val [index parameter]
  (get-dial-val (nth dials index) parameter))

(defn set-dial-by-id [dial-set id value]
  (let [dial-index (.indexOf (get-dial-ids dial-set) id)]
    (swap! (get-values dial-set) (fn [values] (assoc dial-index value)))))

(def cur-pad (atom 0))
(defn handle-dial [channel dial value]
  (cond
    ((and (>= dial (first dialIDs)) (<= dial (last dialIDs))))
      (do (set-dial-by-id (nth dials cur-pad) dial value)
      (print (nth dials cur-pad)))
    :else (throw (Exception. "Invalid dial."))))

(overtone.live/on-event [:midi :control-change]
  (fn [{controller-number :note velocity :data1 data :velocity}]
    (println controller-number velocity data)
  ) ::control-handler)

(overtone.live/on-event [:midi :note-on]
  (fn [m]
    (let [note (:note m)]
      (println (overtone.live/midi->hz note) (:velocity-f m)))) ::note-handler)

(defn -main [] ())
