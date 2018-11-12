(ns checkers.server
  (:require [checkers.utils :refer :all]
            [clojure.edn :as edn]
            [quil.core :as q])
  (:import (java.net InetSocketAddress ConnectException StandardSocketOptions)
           (java.nio.channels ServerSocketChannel SocketChannel)
           (java.nio ByteBuffer)
           (java.io IOException)))

(defonce server-chan (atom nil))
(defonce sock-chan (atom nil))
(def BUFFER_SIZE (int 64))

(defn ^:private set-socket-config [socket]
  (doto socket
    (.configureBlocking false)
    (.setOption StandardSocketOptions/SO_KEEPALIVE true)
    (.setOption StandardSocketOptions/SO_SNDBUF BUFFER_SIZE)
    (.setOption StandardSocketOptions/SO_RCVBUF BUFFER_SIZE)))

(defn byte-buffer->msg [buffer]
  (edn/read-string (apply str (map char (.array buffer)))))

(defn msg->byte-buffer [msg]
  (ByteBuffer/wrap (byte-array (map byte (subs (format (str "%" BUFFER_SIZE "s") msg) 0 BUFFER_SIZE)))))

(defn ^:private test-connection []
  (try
    (.write @sock-chan (msg->byte-buffer [::ping]))
    true
    (catch Exception _ false)))

(defn is-connected? []
  (and (not (nil? @sock-chan))
       (.isOpen @sock-chan)
       (.isConnected @sock-chan)
       (or (not (zero? (mod (q/millis) 5000)))
           (test-connection))))

(defn close-connection! []
  (println "Connection close!")
  (when @server-chan
    (.close @server-chan)
    (reset! server-chan nil))
  (when @sock-chan
    (.close @sock-chan)
    (reset! sock-chan nil)))

(defn ^:private read-msgs! []
  (try
    (if (is-connected?)
      (loop [msgs []]
        (let [buffer (ByteBuffer/allocate BUFFER_SIZE)]
          (if (pos? (.read @sock-chan buffer))
            (recur (conj msgs (byte-buffer->msg buffer)))
            msgs)))
      [])
    (catch IOException e (do (close-connection!)
                             []))))

(defn ^:private write-msg! [msg]
  (try
    (when (is-connected?)
      (println "write: " msg)
      (.write @sock-chan (msg->byte-buffer msg)))
    (catch IOException e (do (println (ex-data e))
                             (close-connection!)))))

(defn start-server! []
  (reset! server-chan (doto (ServerSocketChannel/open) (.bind (InetSocketAddress. port))))
  (println "Waiting on client to connect...")
  (future (reset! sock-chan (set-socket-config (.accept @server-chan)))
          (println "Client connected!")))

(defn connect-to-server! [host]
  (try
    (reset! sock-chan (set-socket-config (SocketChannel/open (InetSocketAddress. ^String host ^Integer port))))
    (write-msg! [::connected])
    (println "Connected")
    (catch ConnectException e (println "Unable to connect to" port))))

(defn host-game! [state]
  (start-server!)
  (assoc state :player :red :game-state :waiting-for-opponent :multiplayer true :hosting? true))

(defn join-game! [state]
  (connect-to-server! (:ip state))
  (assoc state :player :black :game-state :in-progress :multiplayer true :hosting? false
               :board (vec (reverse (:board state)))))

(defmulti handle-msg (fn [_ [k _]] (println "Read: " k) k))
(defmethod handle-msg ::connected [state _] (assoc state :game-state :in-progress))
(defmethod handle-msg ::move [state [_ {:keys [from to]}]]
  (update state :board
          (fn [board]
            (-> board
                (assoc (- 63 from) EMPTY
                       (- 63 to) (get-nth board (- 63 from)))))))
(defmethod handle-msg ::jump [state [_ {:keys [jumped-square]}]]
  (update state :board assoc (- 63 jumped-square) EMPTY))
(defmethod handle-msg ::turn-switch [state _]
  (update state :actions conj [:turn-switch]))
(defmethod handle-msg ::quit [state _]
  (close-connection!)
  ;(assoc state :game-state :opponent-resigned)
  (update state :actions conj [:reset-game]))
(defmethod handle-msg ::ping [state _] state)

(defmulti write-msg (fn [k & _] k))
(defmethod write-msg ::move [_ m]
  (write-msg! [::move m]))
(defmethod write-msg ::jump [_ m]
  (write-msg! [::jump m]))
(defmethod write-msg ::turn-switch [_]
  (write-msg! [::turn-switch]))
(defmethod write-msg ::quit [_]
  (write-msg! [::quit])
  (close-connection!))

(defn update-state [state]
  (reduce
    handle-msg
    state
    (read-msgs!)))


(defmulti perform-action (fn [_ [k _]] k))

(defmethod perform-action :move [state [_ {:keys [release-square square]}]]
  (write-msg ::move {:from square :to release-square})
  state)

(defmethod perform-action :jump [state [_ {:keys [jumped-square]}]]
  (write-msg ::jump {:jumped-square jumped-square})
  state)

(defmethod perform-action :turn-switch [{:keys [player turn] :as state} _]
  (when (and (= player turn) (:multiplayer state))
    (write-msg ::turn-switch))
  state)

(defmethod perform-action :opponent-disconnected [state _]
  (close-connection!)
  (assoc state :multiplayer false :new-menu :player-disconnected-menu :game-state :menu))

(defmethod perform-action :default [state _] state)

(comment
  (start-server!)
  (connect-to-server! "localhost")
  (is-connected?)
  (write-msg! "HELLO :)")
  (write-msg! "OMG")
  (write-msg! "OKAYLY DOKALY")
  (write-msg! "HELLO :)")
  (write-msg! {:msg "WOW"})
  (read-msgs!)
  (update-state {:hello true})

  (close-connection!)

  )
