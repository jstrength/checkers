(ns checkers.navigation
  (:require [checkers.utils :refer :all]
            [quil.core :as q]
            [checkers.draw :as draw]))

(defn get-next-item [item items]
  (nth items (mod (inc (.indexOf items item)) (count items))))

(defn get-previous-item [item items]
  (nth items (mod (dec (.indexOf items item)) (count items))))

(defn get-next-menu [item-idx items-count]
  (mod (inc item-idx) items-count))

(defn get-previous-menu [item-idx items-count]
  (mod (dec item-idx) items-count))

(defn default-action [state _] state)

(declare settings-menu multiplayer-menu pause-menu)

(def main-menu
  {:items [{:id :start-game
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Start Game" (zero? (get-in state [:current-menu :selected-item]))))
            :action (fn [state _]
                      (assoc state :game-state :in-progress))}
           {:id :multiplayer
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Multiplayer" (= 1 (get-in state [:current-menu :selected-item]))))
            :action (fn [state _] (assoc state :current-menu multiplayer-menu))}
           {:id :settings
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Settings" (= 2 (get-in state [:current-menu :selected-item]))))
            :action (fn [state _] (assoc state :current-menu settings-menu))}
           {:id :quit
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Quit" (= 3 (get-in state [:current-menu :selected-item]))))
            :action (fn [_ _] (System/exit 0))}]
   :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
             (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
               (if (= enter-key key-code)
                 (item-action-fn state event)
                 state)))
   :selected-item 0})

(def settings-menu
  {:items [{:id :sound
            :draw (fn [state x y]
                    (draw/draw-menu-item
                      x y
                      (format "Sound: %s" (if (:sound-on? state) "ON" "OFF"))
                      (zero? (get-in state [:current-menu :selected-item]))))
            :action (fn [state _] (update state :sound-on? not))}
           {:id :dark
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Dark:" (= 1 (get-in state [:current-menu :selected-item])))
                    (draw/draw-static-piece (+ x (q/text-width "Dark:  ")) (- y 15) (:dark-color state)))
            :action (fn [state {:keys [key-code]}]
                      (update state :dark-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [color _] color)) ;todo fix or remove
                              (vec (keys dark-colors))))}
           {:id :light
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Light:" (= 2 (get-in state [:current-menu :selected-item])))
                    (draw/draw-static-piece (+ x (q/text-width "Light:  ")) (- y 15) (:light-color state)))
            :action (fn [state {:keys [key-code]}]
                      (update state :light-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [c _] c))
                              (vec (keys light-colors))))}
           {:id :dark-square
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Dark:" (= 3 (get-in state [:current-menu :selected-item])))
                    (draw/draw-square (+ x (q/text-width "Dark: ")) (- y SQUARE_WIDTH) (:dark-square-color state)))
            :action (fn [state {:keys [key-code]}]
                      (update state :dark-square-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [c _] c)) ;todo fix or remove
                              (vec (keys dark-colors))))}

           {:id :light-square
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Light:" (= 4 (get-in state [:current-menu :selected-item])))
                    (draw/draw-square (+ x (q/text-width "Light: ")) (- y SQUARE_WIDTH) (:light-square-color state)))
            :action (fn [state {:keys [key-code]}]
                      (update state :light-square-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [c _] c))
                              (vec (keys light-colors))))}]
   :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
             (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
               (cond
                 (#{right-key left-key} key-code)
                 (item-action-fn state event)
                 (= key-code backspace-key)
                 (assoc state :current-menu
                              (if (= :paused (:game-state state))
                                (assoc pause-menu :selected-item 1)
                                (assoc main-menu :selected-item 2)))
                 :else state)))
   :selected-item 0})

(def host-game-menu
  {:items [{:id :port
            :draw
            (fn [state x y]
              (draw/draw-menu-item x y (format "Port: %s" (:port state)) (zero? (get-in state [:current-menu :selected-item]))))
            :action
            (fn [state key-code]
              (update state :port (fn [port k] (re-find #"[0-9]{0,4}" (str port k))) (name (:key key-code))))}]
   :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
             (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
               (condp = key-code
                 backspace-key
                 (assoc state :current-menu (assoc multiplayer-menu :selected-item 1))
                 (item-action-fn state event))))
   :selected-item 0})

(def join-game-menu
  {:items [{:id :ip
            :draw
            (fn [state x y]
              (draw/draw-menu-item x y "IP: " (zero? (get-in state [:current-menu :selected-item]))))}
           {:id :port
            :draw
            (fn [state x y]
              (draw/draw-menu-item x y "Port: " (= 1 (get-in state [:current-menu :selected-item]))))}]
   :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
             (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
               (condp = key-code
                 backspace-key
                 (assoc state :current-menu (assoc multiplayer-menu :selected-item 0))
                 (item-action-fn state event))))
   :selected-item 0})

(def multiplayer-menu
  {:items [{:id :host-game
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Join Game" (zero? (get-in state [:current-menu :selected-item]))))
            :action (fn [state _] (assoc state :current-menu join-game-menu))}
           {:id :join-game
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Host Game" (= 1 (get-in state [:current-menu :selected-item]))))
            :action (fn [state _] (assoc state :current-menu host-game-menu))}]
   :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
             (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
               (condp = key-code
                 enter-key (item-action-fn state event)
                 backspace-key (assoc state :current-menu (assoc main-menu :selected-item 1))
                 state)))
   :selected-item 0})

(def pause-menu
  {:items [{:id :resume-game
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Resume Game" (zero? (get-in state [:current-menu :selected-item]))))
            :action (fn [state _]
                      (assoc state :game-state :in-progress))}
           {:id :settings
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Settings" (= 1 (get-in state [:current-menu :selected-item]))))
            :action (fn [state _] (assoc state :current-menu settings-menu))}
           {:id :quit
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Quit" (= 2 (get-in state [:current-menu :selected-item]))))
            :action (fn [_ _] (System/exit 0))}]
   :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
             (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
               (if (= enter-key key-code)
                 (item-action-fn state event)
                 state)))
   :selected-item 0})

(defn handle-nav [{:keys [current-menu] :as state} event]
  (condp = (:key-code event)
    up-key
    (update-in state [:current-menu :selected-item]
               get-previous-menu (count (:items current-menu)))
    down-key
    (update-in state [:current-menu :selected-item]
               get-next-menu (count (:items current-menu)))
    ((:action current-menu) state event)))
