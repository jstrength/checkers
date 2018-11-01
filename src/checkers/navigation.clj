(ns checkers.navigation
  (:require [checkers.utils :refer :all]
            [quil.core :as q]
            [checkers.draw :as draw])
  (:import (java.net InetAddress)
           (java.util Date)))

(defn get-next-item [item items]
  (nth items (mod (inc (.indexOf items item)) (count items))))

(defn get-previous-item [item items]
  (nth items (mod (dec (.indexOf items item)) (count items))))

(defn default-action [state _] state)

(defn selected-item? [state item-id]
  (= item-id (get-in state [:current-menu :selected-item])))

(def back-text "⬅Back")

(declare settings-menu multiplayer-menu pause-menu) ;;todo use lookup

(def main-menu
  (let [display-order [:start-game :multiplayer :settings :quit]]
    {:display-order display-order
     :items {:start-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Start Game" (selected-item? state :start-game)))
              :action (fn [state _]
                        (assoc state :game-state :in-progress))}
             :multiplayer
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Multiplayer" (selected-item? state :multiplayer)))
              :action (fn [state _] (assoc state :current-menu multiplayer-menu))}
             :settings
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Settings" (selected-item? state :settings)))
              :action (fn [state _] (assoc state :current-menu settings-menu))}
             :quit
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Quit" (selected-item? state :quit)))
              :action (fn [_ _] (System/exit 0))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (if (= enter-key key-code)
                   (item-action-fn state event)
                   state)))
     :selected-item (first display-order)}))

(def settings-menu
  (let [display-order [:back :sound :dark-piece :light-piece :dark-square :light-square]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state {:keys [key-code] :as event}]
                ;todo any key triggers this guy
                (assoc state :current-menu
                             (assoc
                               (if (= :paused (:game-state state))
                                 pause-menu
                                 main-menu)
                               :selected-item :settings)))}
             :sound
             {:draw (fn [state x y]
                      (draw/draw-menu-item
                        x y
                        (format "Sound: %s" (if (:sound-on? state) "ON" "OFF")) ;todo better key handling
                        (selected-item? state :sound)))
              :action (fn [state _] (update state :sound-on? not))}
             :dark-piece
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Dark:" (selected-item? state :dark-piece))
                      (draw/draw-static-piece (+ x (q/text-width "Dark:  ")) (- y 15) (:dark-color state)))
              :action (fn [state {:keys [key-code]}]
                        (update state :dark-color
                                (condp = key-code
                                  left-key get-previous-item
                                  right-key get-next-item
                                  (fn [color _] color)) ;todo fix or remove
                                (vec (keys dark-colors))))}
             :light-piece
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Light:" (selected-item? state :light-piece))
                      (draw/draw-static-piece (+ x (q/text-width "Light:  ")) (- y 15) (:light-color state)))
              :action (fn [state {:keys [key-code]}]
                        (update state :light-color
                                (condp = key-code
                                  left-key get-previous-item
                                  right-key get-next-item
                                  (fn [c _] c))
                                (vec (keys light-colors))))}
             :dark-square
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Dark:" (selected-item? state :dark-square))
                      (draw/draw-square (+ x (q/text-width "Dark: ")) (- y SQUARE_WIDTH) (:dark-square-color state)))
              :action (fn [state {:keys [key-code]}]
                        (update state :dark-square-color
                                (condp = key-code
                                  left-key get-previous-item
                                  right-key get-next-item
                                  (fn [c _] c)) ;todo fix or remove
                                (vec (keys dark-colors))))}

             :light-square
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Light:" (selected-item? state :light-square))
                      (draw/draw-square (+ x (q/text-width "Light: ")) (- y SQUARE_WIDTH) (:light-square-color state)))
              :action (fn [state {:keys [key-code]}]
                        (update state :light-square-color
                                (condp = key-code
                                  left-key get-previous-item
                                  right-key get-next-item
                                  (fn [c _] c))
                                (vec (keys light-colors))))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def host-game-menu
  (let [display-order [:back :your-ip :port]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state {:keys [key-code] :as event}]
                (cond-> state
                  (= enter-key key-code) (assoc :current-menu (assoc multiplayer-menu :selected-item :host-game))))}
             :your-ip
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x (- y 50) (format "Your IP:\n%s" (.getHostAddress (InetAddress/getLocalHost)))
                                     (selected-item? state :your-ip)))}
             :port
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y (format "Port: %s" port) (selected-item? state :port)))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def join-game-menu
  (let [display-order [:back :ip :port]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state {:keys [key-code] :as event}]
                (cond-> state
                  (= enter-key key-code) (assoc :current-menu (assoc multiplayer-menu :selected-item :join-game))))}
             :ip
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x (- y 50) (format "IP:\n%s" (:ip state))
                                     (selected-item? state :ip)))
              :action
              (fn [state {:keys [key key-code] :as event}]
                (cond-> state
                  (= backspace-key key-code) (update :ip (fn [ip] (cond-> ip (< 0 (count ip)) (subs 0 (dec (count ip))))))
                  :else (update :ip (fn [ip k] (re-find #"[0-9\.]{0,15}" (str ip k))) (name key))))}
             :port
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y (format "Port: %s" port) (selected-item? state :port)))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (item-action-fn state event)))
     :selected-item (first display-order)}))

(def multiplayer-menu
  (let [display-order [:back :host-game :join-game]]
    {:display-order display-order
     :items {:back
             {:draw
              (fn [state x y]
                (draw/draw-menu-item x y back-text (selected-item? state :back)))
              :action
              (fn [state {:keys [key-code] :as event}]
                (cond-> state
                  (= enter-key key-code) (assoc :current-menu (assoc main-menu :selected-item :multiplayer))))}
             :host-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Host Game" (selected-item? state :host-game)))
              :action (fn [state _] (assoc state :current-menu host-game-menu))}
             :join-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Join Game" (selected-item? state :join-game)))
              :action (fn [state _] (assoc state :current-menu join-game-menu))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (condp = key-code
                   enter-key (item-action-fn state event)
                   state)))
     :selected-item (first display-order)}))

(def pause-menu
  (let [display-order [:resume-game :settings :quit]]
    {:display-order display-order
     :items {:resume-game
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Resume Game" (selected-item? state :resume-game)))
              :action (fn [state _]
                        (assoc state :game-state :in-progress
                                     :start-time (.getTime (Date.))))}
             :settings
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Settings" (selected-item? state :settings)))
              :action (fn [state _] (assoc state :current-menu settings-menu))}
             :quit
             {:draw (fn [state x y]
                      (draw/draw-menu-item x y "Quit" (selected-item? state :quit)))
              :action (fn [_ _] (System/exit 0))}}
     :action (fn [{:keys [current-menu] :as state} {:keys [key-code] :as event}]
               (let [item-action-fn (get-in current-menu [:items (:selected-item current-menu) :action] default-action)]
                 (if (= enter-key key-code)
                   (item-action-fn state event)
                   state)))
     :selected-item (first display-order)}))

(defn handle-nav [{:keys [current-menu] :as state} event]
  (condp = (:key-code event)
    up-key
    (update-in state [:current-menu :selected-item] get-previous-item (:display-order current-menu))
    down-key
    (update-in state [:current-menu :selected-item] get-next-item (:display-order current-menu))
    ((:action current-menu) state event)))
