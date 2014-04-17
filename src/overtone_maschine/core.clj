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

(def maschineDials (vector 1  2  3  4  5  6   7  8))
(def rolandDials   (vector 4  5  6  7  8  9  10 11))
(def dialIDs maschineDials)

(defn create-dials [num] (init-vector num (fn [i]
  (vector
    (vector       "pitch" "volume" "pace" "duration" "attack" "decay" "sustain" "release") ;; parameter
    (vector       7       7        7      6          6        6       6         6)         ;; channel
    dialIDs                                                                                ;; dial ID
    (atom (vector 60      100      64     64         64       64      64        64))))))   ;; value

(def dials (atom (create-dials num-pads)))

(def cur-offset (atom 0))
(defn set-samples-offset [start]
  (if (and (= (math/floor start) start) (not (= @cur-offset start)))
    (dosync
      (swap! samples (fn [smps] (map overtone.live/sample (wrap-take start num-pads @audio-files))))
      (swap! dials (fn [dls] (wrap-take start num-pads @dials)))
      (swap! cur-offset (fn [off] start)))))

(defn modulate [value] (math/floor (/ value 5)))

(defn rotate-samples [coarse fine]
  (let [c (* 10 (modulate coarse))
        f (modulate fine)]
    (set-samples-offset (+ c f))))

(defn update-samples-files [files]
  (if (> 0 (count files)) (dosync
    (swap! audio-files (fn [afs] (vec files)))
    (swap! dials (fn [d] (create-dials (count @audio-files))))
    (rotate-samples 0 0))))

(defn get-dial-ids [dial-set] (nth dial-set 2))
(defn get-values [dial-set] (nth dial-set 3))
(defn get-dial-names [dial-set] (nth dial-set 0))

(defn get-dial-val [dial-set dial]
  (nth @(get-values dial-set) (.indexOf (get-dial-names dial-set) dial)))

(defn get-param-val [index parameter]
  (get-dial-val (nth @dials index) parameter))

(defn set-dial-by-id [dial-set id value]
  (let [dial-index (.indexOf (get-dial-ids dial-set) id)]
    (swap! (get-values dial-set) (fn [values] (assoc values dial-index value)))))

(defn clip [minimum maximum value] (max (min value maximum) minimum))
(defn scale [minimum maximum value] (math/floor (+ minimum (* (/ value 127) maximum))))

(def shift (atom :off))
(def solo (atom :off))
(def mute (atom :off))
(def note-repeat (atom :off))
(defn muted [] (and (not @solo) @mute))

(defn enabled [param] (or (= @param :on) (= @param :locked)))
(defn toggle [param to]
  (let [transform (fn [p]
    (if (or (not (= p to)) (enabled shift))
      (cond
        (= p :off) (if (enabled shift) :locked :on)
        (= p :on)  (if (and (not (= param shift)) (enabled shift)) :locked :off)
        (= p :locked) :on)
      p))]
    (swap! param transform)))

(defn translate-pad [pad]
  (.indexOf (vector 12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3) pad))

(defn get-time [index]
  (scale 150 17500 (get-param-val index "pace")))

(def fine-sample-dial (atom 0))
(def coarse-sample-dial (atom 0))
(def cur-pad (atom 0))
(defn handle-dial [dial value]
  (cond
    (and (>= dial (first dialIDs)) (<= dial (last dialIDs)))
      (do (set-dial-by-id (nth @dials @cur-pad) dial value)
          (println (get-values (nth @dials @cur-pad))))
    (or (= dial 101) (= dial 102))
      (let [[coarse fine sample-dial]
            (if (= dial 101)
              [value @fine-sample-dial coarse-sample-dial]
              [@coarse-sample-dial value fine-sample-dial])]
        (dosync (rotate-samples coarse fine)
          (swap! sample-dial (fn [f] value))))
    :else (throw (Exception. (format "Invalid dial: %s" dial)))))

(defn play-sample-vol [pad volume]
  (let [sample (nth @samples pad)]
    (do (println cur-offset sample)
      (overtone.live/sample-player sample)))) ;; currently ignoring dials and volume

(defn handle-toggles [pad to]
  (cond
    (= pad 127) (toggle note-repeat to)
    (= pad 93)  (toggle shift to)
    (= pad 102) (toggle mute to)
    (= pad 30)  (toggle solo to)))

(def pool (at-at/mk-pool))
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
                  (if (enabled note-repeat)
                    (at-at/after (get-time @mpad) (fn [] (handle-pad @mpad velocity false)) pool)))))
      :else (handle-toggles @mpad :on))))

(overtone.live/on-event [:midi :control-change]
  (fn [{controller-number :note velocity :data1 data :velocity}]
    (handle-dial controller-number data)
  ) ::control-handler)

(overtone.live/on-event [:midi :note-on]
  (fn [m]
    (let [note (:note m)
          velocity (:velocity-f m)]
      (handle-pad note velocity true))) ::note-on-handler)

(overtone.live/on-event [:midi :note-off]
  (fn [m]
    (let [note (:note m)]
      (handle-toggles note :off))) ::note-off-handler)

(defn issue-escape-code [code]
  (def esc "\033[")
  (print (clojure.string/join "" [esc code])))

(defn set-cursor [x y]
  (issue-escape-code (clojure.string/join "" [y ";" x ";f"])))

(defn clear-screen []
  (issue-escape-code "2J")
  (set-cursor 0 0))

(defn render []
  (dosync
    (clear-screen)
    (def columns (->> (clojure.java.shell/sh "/bin/sh" "-c" "stty -a < /dev/tty") :out
      (re-find #"columns (\d+)") second))
    (set-cursor 50 0)
    (println (clojure.string/join "" [columns " testing " (System/currentTimeMillis)]))))

(def frames-per-sec 35)
(import java.io.File)
(defn -main [& args]
  (let [files (mapcat (fn [path] (file-seq (File. path))) args)
        paths (map (fn [f] (.getAbsolutePath f)) files)
        lends-with (fn [s substr] (.endsWith (clojure.string/lower-case s) substr))]
    (update-samples-files (filter (fn [p] (or (lends-with p ".wav") (lends-with p ".aif"))) paths))))
;;  (at-at/every (/ 1000 frames-per-sec) render pool))
