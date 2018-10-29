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

(declare settings-menu multiplayer-menu)

(def main-menu
  {:items [{:id :start-game
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Start Game" (zero? (get-in state [:current-menu :selected-item]))))
            :action (fn [state _] (assoc state :game-state :in-progress))}
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
   :back-action identity
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
                    (draw/draw-menu-item x y "Dark Color:" (= 1 (get-in state [:current-menu :selected-item])))
                    (draw/draw-static-piece (+ x (q/text-width "Dark Color:  ")) (- y 15) (:dark-color state)))
            :action (fn [state key-code]
                      (update state :dark-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [c _] c)) ;todo fix or remove
                              (vec (keys dark-colors))))}
           {:id :light
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Light Color:" (= 2 (get-in state [:current-menu :selected-item])))
                    (draw/draw-static-piece (+ x (q/text-width "Light Color:  ")) (- y 15) (:light-color state)))
            :action (fn [state key-code]
                      (update state :light-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [c _] c))
                              (vec (keys light-colors))))}
           {:id :dark-square
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Dark Square:" (= 3 (get-in state [:current-menu :selected-item])))
                    (draw/draw-square (+ x (q/text-width "Dark Square: ")) (- y SQUARE_WIDTH) (:dark-square-color state)))
            :action (fn [state key-code]
                      (update state :dark-square-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [c _] c)) ;todo fix or remove
                              (vec (keys dark-colors))))}

           {:id :light-square
            :draw (fn [state x y]
                    (draw/draw-menu-item x y "Light Square:" (= 4 (get-in state [:current-menu :selected-item])))
                    (draw/draw-square (+ x (q/text-width "Light Square: ")) (- y SQUARE_WIDTH) (:light-square-color state)))
            :action (fn [state key-code]
                      (update state :light-square-color
                              (condp = key-code
                                left-key get-previous-item
                                right-key get-next-item
                                (fn [c _] c))
                              (vec (keys light-colors))))}]
   :back-action (fn [state]
                  (assoc state :current-menu (assoc main-menu :selected-item 2)))
   :selected-item 0})

(def host-game-menu
  {:host-game-menu {:items [:port]}})

(def join-game-menu
  {:join-game-menu {:items [:ip :port]}})

(def multiplayer-menu
  {:items [:name :host-game :join-game]
   :item-actions {}})

(def pause-menu
  {:items [{:id :resume-game
            :display "Resume Game"}
           {:id :settings}
           {:id :quit}]})

(defn handle-nav [{:keys [current-menu] :as state} event]
  (condp = (:key-code event)
    up-key
    (update-in state [:current-menu :selected-item]
               get-previous-menu (count (:items current-menu)))
    down-key
    (update-in state [:current-menu :selected-item]
               get-next-menu (count (:items current-menu)))
    enter-key
    (let [action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
      (action-fn state enter-key))
    right-key
    (let [action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
      (action-fn state right-key))
    left-key
    (let [action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
      (action-fn state left-key))
    backspace-key
    (let [back-action (:back-action current-menu)]
      (back-action state))
    state))
