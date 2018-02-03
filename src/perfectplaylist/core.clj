;------------------------------;
;  THE PERFECT PLAYLIST MAKER  ;
;------------------------------;
;                              ;
;  -----  HOW IT WORKS  -----  ;
;                              ;
;Provide a desired playlist    ; 
;length in seconds, and a list ;
;of songs...                   ;
;A perfect playlist will be    ;
;generated for you!            ;
;                              ;
;------------------------------;

(ns perfectplaylist.core
  (:gen-class))

(defn getAllStdin []
  (take-while identity (repeatedly #(read-line))))

(defn tokenizeLines [lines]
  "join lines into one big line, then tokenize the big line"
  (let [bigLine (clojure.string/join '" " lines)]
    (clojure.string/split bigLine #" ")))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn tokensToSongDurationMap [tokens]
  ;put tokens into map; ( k v k v k v) -> { k v, k v}
  (let [songDurationMap (apply hash-map tokens)]
    ;convert all the values to numbers
    (apply merge (map (fn [[k v]] {k (parse-int v) }) songDurationMap))))

(defn createPlaylist [namesAndDurations targetDuration]
  "add random songs to playlist until no more songs can fit in the target duration"
  (loop [playlist {} viableSongs namesAndDurations]
    (let [currentDuration (reduce + (vals playlist))]
      (let [durationGap (- targetDuration currentDuration)]
        (let [viableSongs  ;remove songs too long to add to the playlist
              (into (hash-map) (filter #(< (second %) durationGap) viableSongs ))]
          ;are there any songs short enough to add to the playlist?
          (if (zero? (count viableSongs))
            playlist
            ;pick random song
            (let [randomSongName (rand-nth (keys viableSongs))]
              (recur 
               ;add the random song to the playlist
               (assoc playlist randomSongName (get viableSongs randomSongName))
               ;remove the chosen song from the list of remaining songs
               (dissoc viableSongs randomSongName)))))))))

(defn -main [& args]
  (let [targetDuration (parse-int (first args))]
    (let [songDurationMap (tokensToSongDurationMap (tokenizeLines (getAllStdin)))]

      ;create a thousand playlists
      (let [listOfPlaylists (repeatedly 1000 #(createPlaylist songDurationMap targetDuration))]

        ;calculate durations of all 1000 playlists
        (let [durations (map (fn [playlist] (reduce + (vals playlist))) listOfPlaylists)]

          (let [playlistsAndDurations (zipmap listOfPlaylists durations)]

            ;get the longest playlist from the 1000
            (let [ longestPlaylist (key (apply max-key val playlistsAndDurations))]
              (doseq [songTitle longestPlaylist] 
                (println (first songTitle))))))))))
