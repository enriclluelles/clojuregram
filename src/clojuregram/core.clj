(ns clojuregram.core
  (:require [clj-http.client :as client])
  (:import (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)))

(def ^:dynamic *credentials* nil)
(def ^:dynamic *access-token* nil)

(defmacro with-access-token
  [access-token & body]
  `(binding [*access-token* ~access-token]
     ~@body))

(defmacro with-credentials
  [credentials & body]
  `(binding [*credentials* ~credentials]
     ~@body))

(defmacro with-access-token-and-credentials
  [access-token credentials & body]
  `(binding [*credentials* ~credentials
             *access-token* ~access-token]
     ~@body))

(defn- subs-uri
  [uri original-params]

  (loop [matches (re-seq #"\{\:(\w+)\}" uri)
         ^String result uri
         params original-params]
    (if (empty? matches) [result params]
      (let [[token kw] (first matches)
            kw (keyword kw)
            value (get params kw)
            rest-uri (.replace result token (str value))
            rest-params (dissoc params kw)]
        (if-not value (throw (Exception. (format "%s needs :%s param to be supplied" uri kw))))
        (recur (rest matches) rest-uri rest-params)))))

(defn- underscore-keys
  [themap]
  (zipmap (map #(keyword (.replace (name %) "-" "_")) (keys themap))
          (vals themap)))

(defn access-token-url
  [credentials]
  (let [default-params {:response_type "code"}
        params (underscore-keys (merge (default-params credentials)))
        params (dissoc params :client_secret :client_ips)]
    (str "https://instagram.com/oauth/authorize" "?" (client/generate-query-string params))))

(defn get-access-token
  [credentials]
  (let [default-params {:grant_type "authorization_code"}
        params (underscore-keys (merge (default-params credentials)))
        response (client/post "https://instagram.com/oauth/access_token" {:form-params params})]
    (get-in response [:body :data])))

(defn- get-full-args
  [args]
  (if *access-token*
    (assoc args :access_token *access-token*)
    (if (get-in *credentials* [:client_id])
      (assoc args :client_id (get-in *credentials* [:client_id]))
      args)))

(defn secretKeyInst [key mac]
    (SecretKeySpec. (.getBytes key) (.getAlgorithm mac)))

(def hmacSHA256
  (memoize (fn [payload secret]
             (let [mac (Mac/getInstance "HMACSHA256")
                   secretKey (SecretKeySpec. (.getBytes secret) (.getAlgorithm mac))
                   byteResult (-> (doto mac
                                    (.init secretKey))
                                  (.doFinal (.getBytes payload)))
                   stringResult (->> byteResult
                                     (map #(format "%x" %))
                                     (apply str))]
               stringResult))))

(defn- extra-headers
  []
  (if-let [secret (get-in *credentials* [:client_secret])]
    (if-let [ip (get-in *credentials* [:client_ips])]
      {"X-Insta-Forwarded-For" (str ip "|" (hmacSHA256 ip secret))}
      {})
    {}))

(defmacro def-instagram-endpoint
  [endpoint-name method path & default-args]
  (let [key-for-merge (if (= method :get) :query-params :form-params)]
    `(defn ~endpoint-name
       [call-args#]
       (let [args# (merge ~default-args call-args#)
             args# (get-full-args args#)
             [uri# query-args#] (subs-uri (str "https://api.instagram.com/v1/" ~path) args#)]
         (client/request (assoc {:url uri# :method ~method :as :json}
                                ~key-for-merge query-args#
                                :headers (extra-headers)))))))

; User Endpoints
(def-instagram-endpoint get-user :get "users/{:user_id}")
(def-instagram-endpoint get-current-user-feed :get "users/self/feed")
(def-instagram-endpoint get-user-medias :get "users/{:user_id}/media/recent")
(def-instagram-endpoint get-current-user-liked-medias :get "users/self/media/liked")
(def-instagram-endpoint search-users :get "users/search")

; Relationship Endpoints
(def-instagram-endpoint get-followings :get "users/{:user_id}/follows")
(def-instagram-endpoint get-followers :get "users/{:user_id}/followed-by")
(def-instagram-endpoint get-current-user-requested-by :get "users/self/requested-by")
(def-instagram-endpoint get-user-relationship :get "users/{:user_id}/relationship")
(def-instagram-endpoint change-user-relationship :post "users/{:user_id}/relationship")

; Media Endpoints
(def-instagram-endpoint get-media :get "media/{:media_id}")
(def-instagram-endpoint search-medias :get "media/search")
(def-instagram-endpoint get-popular :get "media/popular")

; Comment Endpoints
(def-instagram-endpoint get-comments :get "media/{:media_id}/comments")
(def-instagram-endpoint post-comment :post "media/{:media_id}/comments")
(def-instagram-endpoint remove-comment :delete "media/{:media_id}/comments/{:comment_id}")

; Like Endpoints
(def-instagram-endpoint get-likes :get "media/{:media_id}/likes")
(def-instagram-endpoint post-like :post "media/{:media_id}/likes")
(def-instagram-endpoint remove-like :delete "media/{:media_id}/likes")

; Tag Endpoints
(def-instagram-endpoint get-tag :get "tags/{:tag_name}")
(def-instagram-endpoint get-tagged-medias :get "tags/{:tag_name}/media/recent")
(def-instagram-endpoint search-tags :get "tags/search")

; Location Endpoints
(def-instagram-endpoint get-location :get "locations/{:location_id}")
(def-instagram-endpoint get-location-medias :get "locations/{:location_id}/media/recent")
(def-instagram-endpoint search-location :get "locations/search")
