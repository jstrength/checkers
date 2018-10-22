(ns checkers.server
  (:require [clojure.core.async :as async :refer [go go-loop <! put!]])
  (:import (java.net InetSocketAddress ConnectException StandardSocketOptions)
           (java.nio.channels ServerSocketChannel SocketChannel)
           (java.nio ByteBuffer)))

(def ^:static BUFFER_SIZE 1024)

(defn byte-buffer->msg [buffer]
  (clojure.string/trim (apply str (map char (.array buffer)))))

(defn msg->byte-buffer [msg]
  (ByteBuffer/wrap (byte-array (map byte (subs (format (str "%" BUFFER_SIZE "s") msg) 0 BUFFER_SIZE)))))

(defn server-channel [port read-ch write-ch]
  (go
    (with-open [server-chan (doto (ServerSocketChannel/open)
                              (.setOption StandardSocketOptions/SO_REUSEADDR true)
                              (.bind (InetSocketAddress. port)))
                _ (println "Waiting on client to connect...")
                sock-chan (.accept server-chan)]
      (println "Client connected!")
      (go-loop []
        (println "Server reading")
        (let [buffer (ByteBuffer/allocate BUFFER_SIZE)]
          (.read sock-chan buffer)
          (let [msg (byte-buffer->msg buffer)]
            (println "Server read: " msg)
            (put! read-ch msg)))
        (when (.isConnected sock-chan)
          (recur)))
      (go-loop []
        (let [msg (<! write-ch)]
          (println "Server write: " msg)
          (.write sock-chan (msg->byte-buffer msg))
          (if (= "exit" msg)
            (do (.close sock-chan) (.close server-chan))
            (when (.isConnected sock-chan)
              (recur))))))))

(defn client-channel [host port read-ch write-ch]
  (try
    (with-open [sock-chan (SocketChannel/open (InetSocketAddress. ^String host ^Integer port))]
      (println "Connected")
      (go-loop []
        (println "Client reading")
        (let [buffer (ByteBuffer/allocate BUFFER_SIZE)]
          (.read sock-chan buffer)
          (let [msg (byte-buffer->msg buffer)]
            (println "Client read: " msg)
            (put! read-ch msg)))
        (when (.isConnected sock-chan)
          (recur)))
      (go-loop []
        (let [msg (<! write-ch)]
          (println "Client write: " msg)
          (.write sock-chan (msg->byte-buffer msg)))
        (when (.isConnected sock-chan)
          (recur))))
    (catch ConnectException e (println "Unable to connect to" port))))

(comment
  (def s-read-c (async/chan))
  (def s-write-c (async/chan))
  (def c-read-c (async/chan))
  (def c-write-c (async/chan))
  (server-channel 1525 s-read-c s-write-c)
  (client-channel 1525 c-read-c c-write-c)
  (async/put! c-write-c "YI! wow hi :)")
  (async/put! s-write-c "hey guy :)")
  (go (<! s-read-c))
  (async/put!)
  (async/take! s-read-c println)
  async/>!!
  async/>!

  )