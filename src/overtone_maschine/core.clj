(ns overtone-maschine.core
  (:require [clojure.math.numeric-tower :as math])
  (:require [overtone.at-at :as at-at])
  (:require [overtone.live]))

(defn init-vector [length initializer]
  (loop [i 0 v (transient [])]
    (if (< i length)
      (recur (inc i) (conj! v (initializer i)))
      (persistent! v))))

(def audio-files (atom (vector
  "/Users/Shared/Maschine Library/Samples/One Shots/Strike/Bend ClickStart.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Glitch/Crup Swag.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Acoustic Note/Purehigh D6.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Metal/Metallic Digigong 1.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Noise/Noise ProgHouse.wav"
  "/Users/Shared/Maschine Library/Samples/Instruments/Bass/Cute Monster Samples/CuteMonster f#3.wav"
  "/Users/Shared/Maschine Library/Samples/Instruments/Pad/Dweller Samples/Dweller c5.wav"
  "/Users/Shared/Maschine Library/Samples/Instruments/Mallet/Sansa Samples/Sansa C3.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Distortion/Dist BitBreakup.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Distortion/Dist CircuitBent 1.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Acoustic Note/PianoPick Eb0 Sutekh.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Acoustic Note/Pluck Ab4 Kitchen.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Acoustic Note/Trumpet B4.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Acoustic Note/DistGuitar Gb1.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Glitch/Digi Monolith B.wav"
  "/Users/Shared/Maschine Library/Samples/One Shots/Glitch/Glitch ComeIn.wav")))

(def num-pads (count @audio-files))

(def samples (atom (init-vector num-pads (fn [i]
  (overtone.live/sample (nth @audio-files i))))))

(defn wrap-take [start size v]
  (let [strt (mod start (count v))
        sz (min size (count v))
        end-slice (subvec v strt (min (count v) (+ strt sz)))]
    (into [] (concat end-slice (take (- sz (count end-slice)) v)))))

(defn update-samples [start]
  (swap! samples (fn [smps] (map overtone.live/sample (wrap-take start num-pads @audio-files)))))

(defn update-samples-files [files]
  (swap! audio-files (fn [afs] (vec files)))
  (update-samples 0))

(def maschineDials (vector 1  2  3  4  5  6   7  8))
(def rolandDials   (vector 4  5  6  7  8  9  10 11))
(def dialIDs maschineDials)

(def dials (init-vector num-pads (fn [i]
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
  (nth @(get-values dial-set) (.indexOf (get-dial-names dial-set) dial)))

(defn get-param-val [index parameter]
  (get-dial-val (nth dials index) parameter))

(defn set-dial-by-id [dial-set id value]
  (let [dial-index (.indexOf (get-dial-ids dial-set) id)]
    (swap! (get-values dial-set) (fn [values] (assoc values dial-index value)))))

(defn clip [minimum maximum value]
  (max (min value maximum) minimum))

(defn scale [minimum maximum value]
  (math/floor (+ minimum (* (/ value 127) maximum))))

(defn toggle-shift [param value shiftval]
  (swap! param (fn [p] (or shiftval (> value 0)))))

(def shift (atom false))
(defn toggle [param value]
  (toggle-shift param value @shift))

(def solo (atom false))
(def mute (atom false))
(defn muted [] (and (not @solo) @mute))

(defn translate-pad [pad]
  (.indexOf (vector 12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3) pad))

(defn get-time [index]
  (scale 150 17500 (get-param-val index "pace")))

(def cur-pad (atom 0))
(defn handle-dial [dial value]
  (cond
    (and (>= dial (first dialIDs)) (<= dial (last dialIDs)))
      (do (set-dial-by-id (nth dials @cur-pad) dial value)
          (println (get-values (nth dials @cur-pad))))
    :else (throw (Exception. "Invalid dial."))))

(defn play-sample-vol [pad volume]
  (overtone.live/sample-player (nth @samples pad))) ;; currently ignoring dials and volume

(def note-repeat (atom false))
(defn handle-pad [pad velocity set-cur-pad]
  (with-local-vars [mpad pad]
    (cond
      (and (<= @mpad 15) (>= @mpad 0))
        (do (if set-cur-pad
              (do (var-set mpad (translate-pad @mpad))
                  (swap! cur-pad (fn [p] mpad))))
            (if (> velocity 0)
              (do (if (not (muted))
                    (play-sample-vol @mpad (min (+ 35 velocity) (get-param-val @mpad "volume"))))
                  (if @note-repeat
                    (at-at/after (get-time @mpad) (fn [] (handle-pad @mpad velocity false)))))))
      (= @mpad 127) (toggle note-repeat velocity)
      (= @mpad 93)  (toggle-shift shift velocity false)
      (= @mpad 102) (toggle mute velocity)
      (= @mpad 30)  (toggle solo velocity))))

(overtone.live/on-event [:midi :control-change]
  (fn [{controller-number :note velocity :data1 data :velocity}]
    (handle-dial controller-number data)
  ) ::control-handler)

(overtone.live/on-event [:midi :note-on]
  (fn [m]
    (let [note (:note m)
          channel (:channel m)
          data (:data2-f m)
          velocity (:velocity-f m)]
      (println note channel data velocity)
      (handle-pad note velocity true))
  ) ::note-on-handler)

(overtone.live/on-event [:midi :note-off]
  (fn [m]
    (println m)
  ) ::note-off-handler)

(import java.io.File)
(defn -main [& args]
  (let [files (mapcat (fn [path] (file-seq (File. path))) args)
        paths (map (fn [f] (.getAbsolutePath f)) files)
        lends-with (fn [s substr] (.endsWith (clojure.string/lower-case s) substr))]
    (update-samples-files (filter (fn [p] (or (lends-with p ".wav") (lends-with p ".aif"))) paths))))
