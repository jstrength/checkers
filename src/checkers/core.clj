(ns checkers.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as async]
            [checkers.draw :as draw]
            [checkers.logic :as logic]
            [checkers.utils :refer :all]
            [checkers.navigation :as nav]
            [checkers.server :as server])
  (:import (java.util Date)))

(def read-ch (async/chan 100))
(def write-ch (async/chan 100))

(def starting-state
  (merge default-game-state default-settings-state))

(def last-state (atom starting-state))
(defn notify-opponent [{:keys [multiplayer] :as state}]
  (when multiplayer
    (println (pr-str (update state :board (comp vec reverse))))
    (if (not= @last-state state)
      (reset! last-state state)
      (async/put! write-ch (pr-str (update state :board (comp vec reverse))))))
  state)

(defn read-opponent-msg [{:keys [multiplayer player turn] :as state}]
  (when (and multiplayer (not= player turn))
    (when-let [msg (async/poll! read-ch)]
      ;(println "msg:" msg)
      (let [new-state (read-string msg)]
        ;(println new-state)
        (assoc new-state :player player :waiting (= turn (:turn new-state)))))))

(defn on-close [e]
  (println e)
  (try
    (async/go (async/>! write-ch "exit"))
    (catch Exception _ nil)))

(defn setup [args]
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (q/text-size 30)
  (q/smooth)

  ;(q/text-font (q/create-font "AllStarResort.ttf" 50 true))
  (q/text-font (q/create-font "AndaleMono" 50))

  ; setup function returns initial state. It contains
  ; circle color and position.
  (let [starting-state (assoc starting-state
                         :current-menu nav/main-menu
                         :background-img (q/load-image "background.jpg"))] ;todo move into it's own resource-state instead?
    (case (first args)
      "server" (do (server/server-channel read-ch write-ch)
                   (assoc starting-state :player :red
                                         :multiplayer true))
      "client" (do (server/client-channel (second args) read-ch write-ch)
                   (-> starting-state
                       (update :board (comp vec reverse))
                       (assoc :player :black :multiplayer true)))
      starting-state)))

(defn mouse-pressed [{:keys [game-state turn board jumping-piece] :as state} {:keys [x y] :as event}]
  (if (and (= game-state :in-progress)
           (> (- SCREEN_SIZE MARGIN) x MARGIN)
           (> (- SCREEN_SIZE MARGIN) y MARGIN))
    (let [square (+ (quot (- x MARGIN) SQUARE_WIDTH)
                    (* 8 (quot (- y MARGIN) SQUARE_WIDTH)))]
      ;(println (is-playable-piece? state square))
      ;(println square)
      (if (and (if jumping-piece (= square jumping-piece) true)
               (= turn (if (#{BLACK BLACK_KING} (get board square)) :black :red))
               (logic/is-playable-piece? state square))
        (-> state
            ;(assoc-in [:board square] EMPTY)
            (assoc :selected {:piece (get board square) :square square :x x :y y}))
        state))
    state))

(defn mouse-released [{:keys [board selected] :as state} event]
  (if-let [{:keys [x y piece square]} selected]
    (let [release-square (logic/calc-square-at x y)]
      (if (and (= (get board release-square) EMPTY)
               (let [{x :raw-x y :raw-y} (position-coordinates release-square)]
                 (if (odd? y) (even? x) (odd? x)))
               (seq (filter #(= release-square %) (or (logic/get-valid-jumps state square)
                                                      (logic/get-valid-moves board square))))
               (< MARGIN x (+ BOARD_WIDTH MARGIN))
               (< MARGIN y (+ BOARD_WIDTH MARGIN)))
        ;; dropped piece is on valid square within bounds of board
        (-> state
            (update :actions conj {:type :move :release-square release-square
                                   :square square :piece piece}))
        ;; reset piece to original square
        (-> state
            ;(assoc-in [:board square] piece)
            (dissoc :selected))))
    state))

(defn mouse-dragged [{:keys [selected] :as state} {:keys [x y] :as event}]
  (if selected
    (update state :selected assoc :x x :y y)
    state))

(defn key-pressed [state event]
  (println event)
  (cond
    (#{:paused :menu} (:game-state state))
    (nav/handle-nav state event)

    (= :in-progress (:game-state state))
    (case (:key event)
      :p (assoc state :game-state :paused
                      :current-menu nav/pause-menu)
      :q (merge state default-game-state)
      state)

    :else
    (assoc state :current-menu nav/play-again-menu
                 :game-state :menu)))

(defn handle-multiplayer [{:keys [multiplayer turn player] :as state}]
  (if (and multiplayer (not= turn player))
    (or (read-opponent-msg state) state)
    (dissoc state :waiting)))

(defn handle-actions [{:keys [actions] :as state}]
  ;(println state)
  (reduce
    (fn [s action]
      (logic/perform-action s action))
    (dissoc state :actions)
    (distinct actions)) ;todo why need distinct?/?
  )

(defn handle-turn-timer [state]
  (if (>= (- (.getTime (Date.)) (:start-time state)) 1000)
    (cond->
      (-> state
          (update :total-seconds inc)
          (assoc :start-time (.getTime (Date.))))

      (not (zero? (:timer state)))
      (update :last-turn-seconds inc)

      (zero? (- TURN_TIMER_LIMIT (:last-turn-seconds state)))
      (update :actions conj {:type :turn-switch}))
    state))

(defn update-state [{:keys [game-state multiplayer actions] :as state}]
  (cond->
    state

    true
    notify-opponent

    (= :in-progress game-state)
    handle-turn-timer

    multiplayer
    handle-multiplayer

    actions
    handle-actions))

(defn draw-state [state]
  (if (#{:paused :menu} (:game-state state))
    (doto state (draw/menu))
    (doto state (draw/game))))

(defn -main [& args]
  (q/defsketch checkers-quil
    :title "Checkers"
    :size [SCREEN_SIZE SCREEN_SIZE]
    ; setup function called only once, during sketch initialization.
    :setup (partial setup args)
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :key-pressed key-pressed
    :on-close on-close
    :features [:keep-on-top #_:exit-on-close]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
