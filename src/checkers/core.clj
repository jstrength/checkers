(ns checkers.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as async]
            [checkers.server :as server]))

(def screen-size 500)
(def margin 50)
(def board-width (- screen-size (* 2 margin)))
(def square-width (/ board-width 8))
(def piece-width (* 0.75 square-width))

(def EMPTY 0)
(def BLACK 1)
(def RED 2)
(def BLACK_KING 3)
(def RED_KING 4)


(def read-ch (async/chan))
(def write-ch (async/chan))

(def position-coordinates
  (vec (for [y (range 8)
             x (range 8)]
         {:raw-x x :raw-y y
          :x (+ (* x square-width) margin)
          :y (+ (* y square-width) margin)})))

(def starting-state
  {:board (vec (for [n (range 0 64)]
                 (let [{x :raw-x y :raw-y} (position-coordinates n)]
                   (if (if (odd? y) (even? x) (odd? x))
                     (cond (< n (* 3 8)) BLACK
                           (> n (- 63 (* 3 8))) RED
                           :else EMPTY)
                     EMPTY))))
   :multiplayer false
   :turn :red
   :state :in-progress})

(defn notify-opponent [square release-square {:keys [multiplayer] :as state}]
  (when multiplayer
    (async/put! write-ch (str "move:" square "-" release-square)))
  state)

(defn read-opponent-msg [{:keys [multiplayer player turn] :as state}]
  (when (and multiplayer (not= player turn))
    ;(println player "take!")
    (when-let [msg (async/poll! read-ch)]
      (println "msg:" msg)
      (let [[_ from to] (re-find #"move:(\d+)-(\d+)" msg)]
        (println [from to])
        [(Integer/parseInt from) (Integer/parseInt to)]))))

(defn find-jumped-square [s e]
  (when (> (Math/abs ^Integer (- s e)) 9)
    ;(println "JUMPED: " s e (/ (+ s e) 2))
    (/ (+ s e) 2)))

(defn get-nth [board n]
  (when (>= 63 n 0)
    (nth board n)))

;;user iterate on this maybe??
(defn go-direction [dir pos]
  (case dir
    :top-left
    (when (not (zero? (mod pos 8)))
      (- pos 9))
    :top-right
    (when (not (zero? (mod (inc pos) 8)))
      (- pos 7))
    :bottom-left
    (when (not (zero? (mod pos 8)))
      (+ pos 7))
    :bottom-right
    (when (not (zero? (mod (inc pos) 8)))
      (+ pos 9))))

(defn get-valid-jumps [board pos]
  (letfn [(jumpable? [player dir p]
            (let [something (if (= player :black) #{BLACK BLACK_KING} #{RED RED_KING})]
              (and (some->> p (go-direction dir) (get-nth board) something)
                   (some->> p (go-direction dir) (go-direction dir) (get-nth board) (= EMPTY)))))]
    (seq
      (condp = (get-nth board pos)
        RED
        (cond-> []
                (jumpable? :black :top-right pos)
                (conj (->> pos (go-direction :top-right) (go-direction :top-right)))

                (jumpable? :black :top-left pos)
                (conj (->> pos (go-direction :top-left) (go-direction :top-left))))

        BLACK
        (cond-> []
                (jumpable? :red :bottom-right pos)
                (conj (->> pos (go-direction :bottom-right) (go-direction :bottom-right)))

                (jumpable? :red :bottom-left pos)
                (conj (->> pos (go-direction :bottom-left) (go-direction :bottom-left))))

        EMPTY
        nil
        ;;default to kings
        (let [player (if (= (get-nth board pos) RED_KING) :black :red)]
          (cond-> []
                  (jumpable? player :top-right pos)
                  (conj (->> pos (go-direction :top-right) (go-direction :top-right)))

                  (jumpable? player :top-left pos)
                  (conj (->> pos (go-direction :top-left) (go-direction :top-left)))

                  (jumpable? player :bottom-right pos)
                  (conj (->> pos (go-direction :bottom-right) (go-direction :bottom-right)))

                  (jumpable? player :bottom-left pos)
                  (conj (->> pos (go-direction :bottom-left) (go-direction :bottom-left)))))))))

(defn get-valid-moves [board pos]
  (->> (condp = (get-nth board pos)
         RED
         (cond-> []
                 (some->> (go-direction :top-right pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :top-right pos))

                 (some->> (go-direction :top-left pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :top-left pos)))
         BLACK
         (cond-> []
                 (some->> (go-direction :bottom-right pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :bottom-right pos))

                 (some->> (go-direction :bottom-left pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :bottom-left pos)))
         EMPTY
         nil
         ;;default to kings
         (cond-> []
                 (some->> (go-direction :top-right pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :top-right pos))

                 (some->> (go-direction :top-left pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :top-left pos))

                 (some->> (go-direction :bottom-right pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :bottom-right pos))

                 (some->> (go-direction :bottom-left pos) (get-nth board) (= EMPTY))
                 (conj (go-direction :bottom-left pos))))
       (remove nil?)
       (seq)))

(defn is-playable-piece? [{:keys [board turn player multiplayer] :as state} pos]
  (and (if multiplayer (= turn player) true)
       (or
         (get-valid-jumps board pos) ;;our piece has valid jumps
         (and
           ;;there isn't another jumpable piece
           (not (seq (reduce (fn [c p]
                               (if (= turn (if (#{BLACK BLACK_KING} (get board p)) :black :red))
                                 (into c (get-valid-jumps board p))
                                 c))
                             [] (range 64))))
           (get-valid-moves board pos)) ;; this piece can move
         )))

(defn setup [args]
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (q/text-size 30)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (case (first args)
    "server" (do (server/server-channel 1337 read-ch write-ch)
                 ;todo add side picker to UI
                 (assoc starting-state :player :red
                                       :multiplayer true)
                 )
    "client" (do (server/client-channel 1337 read-ch write-ch)
                 (assoc starting-state :player :black
                                       :multiplayer true))
    starting-state))

(defn mouse-pressed [{:keys [turn board jumping-piece] :as state} {:keys [x y] :as event}]
  (if (and (> (- screen-size margin) x margin)
           (> (- screen-size margin) y margin))
    (let [square (+ (quot (- x margin) square-width)
                    (* 8 (quot (- y margin) square-width)))]
      ;(println (is-playable-piece? board turn square))
      ;(println square)
      (if (and (if jumping-piece (= square jumping-piece) true)
               (= turn (if (#{BLACK BLACK_KING} (get board square)) :black :red))
               (is-playable-piece? state square))
        (-> state
            ;(assoc-in [:board square] EMPTY)
            (assoc :selected {:piece (get board square) :square square :x x :y y}))
        state))
    state))

(defn mouse-released [{:keys [board selected] :as state} event]
  (if-let [{:keys [x y piece square]} selected]
    (let [release-square
          (+ (quot (- x margin) square-width)
             (* 8 (quot (- y margin) square-width)))]
      (if (and (= (get board release-square) EMPTY)
               (let [{x :raw-x y :raw-y} (position-coordinates release-square)]
                 (if (odd? y) (even? x) (odd? x)))
               (seq (filter #(= release-square %) (or (get-valid-jumps board square)
                                                      (get-valid-moves board square))))
               (< margin x (+ board-width margin))
               (< margin y (+ board-width margin)))
        ;; dropped piece is on valid square within bounds of board
        (-> state
            (assoc-in [:board release-square]
                      (cond
                        (and (= piece RED) (< release-square 8))
                        RED_KING
                        (and (= piece BLACK) (< 55 release-square))
                        BLACK_KING
                        :else
                        piece))
            (assoc-in [:board square] EMPTY)
            (update :board (fn [board]
                             (if-let [jumped-square (find-jumped-square square release-square)]
                               (assoc board jumped-square EMPTY)
                               board)))
            (dissoc :selected :jumping-piece)
            ((fn [s]
               (if (and (seq (get-valid-jumps board square))
                        (seq (get-valid-jumps (:board s) release-square)))
                 (assoc s :jumping-piece release-square)
                 (update s :turn #(if (= % :red) :black :red)))))
            ((fn [s]
               (assoc s :state
                        (cond ;todo game is tied
                          (not (seq (filter #{BLACK BLACK_KING} (:board s))))
                          :red-wins
                          (not (seq (filter #{RED RED_KING} (:board s))))
                          :black-wins
                          :else
                          :in-progress))))
            ((partial notify-opponent square release-square)))
        ;; reset piece to original square
        (-> state
            ;(assoc-in [:board square] piece)
            (dissoc :selected))))
    state))

(defn mouse-dragged [{:keys [selected] :as state}
                     {:keys [x y] :as event}]
  (if selected
    (update state :selected assoc :x x :y y)
    state))

(defn update-state [{:keys [multiplayer turn player] :as state}]
  (if (and multiplayer (not= turn player))
    (if-let [[from to] (read-opponent-msg state)]
      (-> state
          (dissoc :waiting)
          (assoc :turn player)
          (update :board (fn [board] (-> board (assoc from EMPTY to (get board from))))))
      (assoc state :waiting true))
    (dissoc state :waiting)))

(defn draw-state [{:keys [board selected turn] :as state}]
  ;board grid
  (q/background 255)
  (q/stroke 0)

  ;;draw game info
  (q/text-size 20)
  (q/fill 0)
  (q/text (str "Turn: " (name turn)) 10 30)
  (when (:player state)
    (if (:waiting state)
      (q/text (str "Waiting for oppoent to move") 150 30)
      (q/text (str "Your turn") 150 30)))
  (when (not= :in-progress (:state state))
    (q/text (name (:state state)) (/ screen-size 2) 30))
  (q/text-size 30)

  ;; draw board
  (doseq [n (range 9)]
    (q/line margin (+ margin (* square-width n)) (+ board-width margin) (+ margin (* square-width n)))
    (q/line (+ margin (* square-width n)) margin (+ margin (* square-width n)) (+ board-width margin)))
  (q/fill 180 120 70)
  (doseq [{:keys [x y raw-x raw-y]} position-coordinates]
    (when (if (odd? raw-y) (even? raw-x) (odd? raw-x))
      (q/rect x y square-width square-width)))

  (when-let [{:keys [x y piece square]} selected]
    ;; highlight valid moves
    (q/no-fill)
    (q/stroke-weight 5)
    (q/with-stroke
      [255 240 0]
      (doseq [s (or (get-valid-jumps board square) (get-valid-moves board square))]
        (let [{:keys [x y]} (position-coordinates s)]
          (q/rect x y square-width square-width))))
    (q/stroke-weight 1)
    ;; draw selected piece
    (if (#{BLACK BLACK_KING} piece)
      (q/fill 0)
      (q/fill 255 0 0))
    (q/ellipse x y piece-width piece-width)

    (condp = piece
      BLACK_KING (do (q/fill 255 0 0) (q/text "K" (+ x -10) (+ y 10)) (q/fill 0))
      RED_KING (do (q/fill 0) (q/text "K" (+ x -10) (+ y 10)) (q/fill 255 0 0))
      nil))

  ;; draw static pieces
  (doall
    (map-indexed
      (fn [pos b]
        (when (and (not= b EMPTY) (not= pos (:square selected)))
          (let [{:keys [x y]} (get-nth position-coordinates pos)]
            (if (#{BLACK BLACK_KING} b)
              (q/fill 0)
              (q/fill 255 0 0))
            (q/ellipse (+ x (/ square-width 2)) (+ y (/ square-width 2))
                       piece-width piece-width)
            (condp = b
              BLACK_KING (do (q/fill 255 0 0) (q/text "K" (+ x -10 (/ square-width 2)) (+ y 10 (/ square-width 2))) (q/fill 0))
              RED_KING (do (q/fill 0) (q/text "K" (+ x -10 (/ square-width 2)) (+ y 10 (/ square-width 2))) (q/fill 255 0 0))
              nil))))
      board)))


(defn -main [& args]
  (q/defsketch checkers-quil
    :title "Checkers"
    :size [screen-size screen-size]
    ; setup function called only once, during sketch initialization.
    :setup (partial setup args)
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :exit-on-close true
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
