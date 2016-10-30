(ns overtone-maschine.core
  (:require [clojure.math.numeric-tower :as math])
  (:require [overtone.at-at :as at-at])
  (:require [overtone.live :as ot.l]))

(def debug-string (atom ""))

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
  (ot.l/sample (nth @audio-files i))))))

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

(def all-dials (atom (create-dials num-pads)))
(def dials (atom @all-dials))

(def cur-offset (atom 0))
(defn set-samples-offset [start]
  (if (and (= (math/floor start) start) (not (= @cur-offset start)))
    (dosync
      (swap! samples (fn [smps] (map ot.l/sample (wrap-take start num-pads @audio-files))))
      (swap! dials (fn [dls] (wrap-take start num-pads @all-dials)))
      (swap! cur-offset (fn [off] start)))))

(defn modulate [value] (math/floor (/ value 3)))

(def samples-per-coarse-tick num-pads)
(def samples-per-fine-tick (/ samples-per-coarse-tick 4))
(def max-files (+ (* samples-per-coarse-tick (modulate 128)) (* samples-per-fine-tick (modulate 128))))
(defn rotate-samples [coarse fine]
  (let [c (* samples-per-coarse-tick (modulate coarse))
        f (* samples-per-fine-tick (modulate fine))]
    (set-samples-offset (+ c f))))

(defn update-samples-files [files]
  (if (> (count files) 0) (dosync
    (swap! audio-files (fn [afs] (vec (take max-files files))))
    (swap! all-dials (fn [d] (create-dials (count @audio-files))))
    (dorun (map (fn [f] (do (print ".") (flush) (ot.l/sample f))) @audio-files))
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
(defn scale-val [minimum maximum value valuemax] (+ minimum (* (/ value valuemax) maximum)))
(defn scale-float [minimum maximum value] (scale-val minimum maximum value 127))
(defn scale [minimum maximum value] (math/floor (scale-float minimum maximum value)))

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

(def pad-map (vector 12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3))
(defn translate-pad [pad] (.indexOf pad-map pad))

(defn get-delay [index]
  (scale 150 17500 (get-param-val index "pace")))

(def fine-sample-dial (atom 0))
(def coarse-sample-dial (atom 0))
(def cur-pad (atom 0))
(defn handle-dial [dial value]
  (cond
    (and (>= dial (first dialIDs)) (<= dial (last dialIDs)))
      (set-dial-by-id (nth @dials @cur-pad) dial value)
    (or (= dial 101) (= dial 102))
      (let [[coarse fine sample-dial]
            (if (= dial 101)
              [value @fine-sample-dial coarse-sample-dial]
              [@coarse-sample-dial value fine-sample-dial])]
        (dosync (rotate-samples coarse fine)
          (swap! sample-dial (fn [f] value))))
    :else (throw (Exception. (format "Invalid dial: %s" dial)))))

(ot.l/definst sampled-inst
  [level 1 rate 1 loop? 0 attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1]
    (let [FREE ot.l/FREE
          env (ot.l/env-gen
                (ot.l/adsr attack decay sustain release level curve) :gate gate :action FREE)]
      (* env (ot.l/scaled-play-buf 2 (first @samples) :rate rate :level level :loop loop? :action FREE))))

(ot.l/defsynth stretch-sample [buf 0 shift 1 pitch 1]
  (let [buff  (ot.l/play-buf 2 buf pitch)
        chain (ot.l/fft (ot.l/local-buf 2048 2) buff)]
       (ot.l/out 0 (ot.l/pan2 (* 0.5 (ot.l/ifft (ot.l/pv-mag-shift chain 1 shift)))))))

(defn distrib [value maximum]
  (let [compute-y (fn [x] (+ 1 (Math/pow (- x 1) 3)))
        y (compute-y (scale-float 0.0 2.0 (double value)))]
    (swap! debug-string (fn [s] (format "value: %s, y: %s" value y)))
    (if (> y 1) (scale-val 1.0 (double maximum) (- y 1.0) 1.0) y)))

(defn play-sample-vol [pad volume]
  (let [sample (nth @samples pad)
        shift (distrib (get-param-val pad "duration") 500)
        pitch (distrib (get-param-val pad "pitch") 10)]
    (do
      ;;(swap! debug-string (fn [s] (format "shift: %s, pitch: %s" shift pitch)))
      (stretch-sample sample shift pitch)))) ;; currently ignoring volume

(defn handle-toggles [pad to]
  (cond
    (= pad 127) (toggle note-repeat to)
    (= pad 93)  (toggle shift to)
    (= pad 102) (toggle mute to)
    (= pad 30)  (toggle solo to)))

(def pad-press (atom [0 0]))
(def pool (at-at/mk-pool))
(defn handle-pad [pad velocity set-cur-pad]
  (with-local-vars [mpad pad]
    (cond
      (and (<= @mpad 15) (>= @mpad 0))
        (do (if set-cur-pad
              (do (var-set mpad (translate-pad @mpad))
                  (swap! cur-pad (fn [p] @mpad))))
            (if (> velocity 0)
              (do (if (not (muted)) (do
                    (if set-cur-pad (swap! pad-press (fn [v] [@mpad (System/currentTimeMillis)])))
                    (play-sample-vol @mpad (min (+ 35 velocity) (get-param-val @mpad "volume")))))
                  (if (enabled note-repeat)
                    (at-at/after (get-delay @mpad) (fn [] (handle-pad @mpad velocity false)) pool)))))
      :else (handle-toggles @mpad :on))))

(ot.l/on-event [:midi :control-change]
  (fn [{controller-number :note velocity :data1 data :velocity}]
    (handle-dial controller-number data)
  ) ::control-handler)

(ot.l/on-event [:midi :note-on]
  (fn [m]
    (let [note (:note m)
          velocity (:velocity-f m)]
      (handle-pad note velocity true))) ::note-on-handler)

(ot.l/on-event [:midi :note-off]
  (fn [m]
    (let [note (:note m)]
      (handle-toggles note :off))) ::note-off-handler)

(defn issue-escape-code [code]
  (def esc "\033[")
  (print (clojure.string/join "" [esc code])))

(defn set-cursor [x y] (issue-escape-code (clojure.string/join "" [(+ y 1) ";" (+ x 1) ";f"])))

(defn print-inverted [sz]
  (issue-escape-code "7m")
  (print sz)
  (issue-escape-code "27m"))

(def rows (atom 0))
(def columns (atom 0))
(defn clear-screen []
  (let [tty-info (clojure.java.shell/sh "/bin/sh" "-c" "stty -a < /dev/tty")]
    (swap! rows (fn [r] (read-string (->> tty-info :out (re-find #"; (\d+) rows;") second))))
    (swap! columns (fn [c] (read-string (->> tty-info :out (re-find #"; (\d+) columns;") second))))
    (issue-escape-code "2J")
    (set-cursor 0 0)))

(defn sum [vals] (reduce + vals))

(defn get-pad-text [i max-width]
  (let [filename (clojure.string/join
                  (drop-last (clojure.string/split (.name (nth @samples i)) #"\."))) 
        idx (format "%02d" (+ 1 i))
        joined (clojure.string/join " " [idx filename])]
    (format "%s " (.substring joined 0 (min (.length joined) (- max-width 1))))))

(defn render-pads [offset-y]
  (let [max-pad-width 25
        pads-per-row 4
        pad-width (min (/ @columns pads-per-row) max-pad-width)
        width-reducer (fn [[acc prev] cur]
                        (let [nxt (+ cur acc)
                              nxt-floor (math/floor nxt)
                              nxt-acc (- nxt nxt-floor)]
                          [nxt-acc nxt-floor]))
        widths (into [] (rest (mapcat rest (reductions width-reducer [0 0] (repeat pads-per-row pad-width)))))
        sum-pads (sum widths)
        offset-x (- @columns sum-pads)
        p-rows (map (partial into []) (partition pads-per-row pad-map))
        row-data (map vector (range (count p-rows)) (map vector (repeat (count p-rows) widths) p-rows))
        print-row (fn [[y [widths pads]]]
                    (let [row-reducer (fn [acc [width pad]]
                                        (set-cursor acc (+ offset-y y))
                                        (def printer (if (= pad @cur-pad) print-inverted print))
                                        (printer (get-pad-text pad width))
                                        (+ acc width))]
                      (reduce row-reducer offset-x (map vector widths pads))))]
    (dorun (map print-row row-data))))

(defn print-at [x y string]
  (set-cursor x y)
  (print string))

(defn render-toggles [offset-y]
  (if (not (= @note-repeat :off)) (print-at 0 offset-y "note-repeat"))
  (if (not (= @solo :off)) (print-at 12 offset-y "solo"))
  (if (not (= @mute :off)) (print-at 17 offset-y "mute")))

(defn render-stats [offset-y]
  (set-cursor 0 offset-y)
  (let [cur-time (System/currentTimeMillis)]
    (print cur-time)
    (set-cursor 0 (+ offset-y 1))
    (print (format "%s samples loaded" (count @audio-files)))
    cur-time))

(defn rpad [size string]
  (.substring (format "%s%s" string (apply str (repeat size " "))) 0 size))

(defn lpad [size string]
  (clojure.string/reverse (rpad size (clojure.string/reverse string))))

(defn render-dials [offset-y]
  (let [dials (nth @dials @cur-pad)
        labels (first dials)
        values (map (fn [v] (lpad (.length (nth v 1)) (.toString (nth v 0)))) (map vector @(nth dials 3) labels))]
    (set-cursor 0 offset-y)
    (print (clojure.string/join " " labels))
    (set-cursor 0 (+ 1 offset-y))
    (print (clojure.string/join " " values))))

(def schedule (atom nil))
(defn render []
  (try
    (dosync
      (clear-screen)
      (let [cur-time (render-stats 3)]
        (render-dials 0)
        (render-pads 3)
        (if (> (nth @pad-press 1) (- cur-time 600)) (print-at 0 6 (get-pad-text (nth @pad-press 0) @columns)))
        (render-toggles 8)
        (if (> (.length @debug-string) 0) (print-at 0 9 (format "Debug: %s" @debug-string)))
        (flush)))
    (catch Exception e (do (println e) (at-at/stop @schedule)))))

(def frames-per-sec 35)
(import java.io.File)
(defn -main [& args]
  (let [files (mapcat (fn [path] (file-seq (File. path))) args)
        paths (map (fn [f] (.getAbsolutePath f)) files)
        lends-with (fn [s substr] (.endsWith (clojure.string/lower-case s) substr))]
    (update-samples-files (filter (fn [p] (or (lends-with p ".wav") (lends-with p ".aif"))) paths)))
  (swap! schedule (fn [s] (at-at/every (/ 1000 frames-per-sec) render pool))))
