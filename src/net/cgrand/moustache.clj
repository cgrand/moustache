;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.moustache
 "Moustache is a micro web framework/internal DSL to wire Ring handlers and middlewares."
 (:require [ring.middleware [params :as p] [keyword-params :as kwp]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn uri-segments
 "Splits the uri of the given request map around / and decode segments." 
 [{:keys [#^String uri]}]
  (rest (map #(java.net.URLDecoder/decode % "UTF-8") (.split uri "/" -1))))
  
(defn uri
 "Turns a seq of decoded segment into an uri."  
 [segments]
  (apply str "/" (interpose "/" (map #(java.net.URLEncoder/encode % "UTF-8") segments))))

(defn- split-route [route default-etc]
  (if-let [[l ll] (rseq route)]
    (cond
      (= '& l) [(pop route) default-etc]
      (= '& ll) [(-> route pop pop) l]
      :else [route nil])
    [[""] nil]))

(def #^{:doc "Handler that causes the framework to fall through the next handler"} 
  pass (constantly nil))

(def #^{:doc "Handler that always return a 404 Not Found status."} 
  not-found (constantly {:status 404}))

(defn alter-request
 "Middleware that passes (apply f request args) to handler instead of request." 
 [handler f & args]
  #(handler (apply f % args)))

(defn alter-response
 "Middleware that returns (apply f response args) instead of response." 
 [handler f & args]
  #(apply f (handler %) args))

(defn- regex? [x]
  (instance? java.util.regex.Pattern x))

(defn- simple-segment? [x]
  (or (string? x) (regex? x) (= '& x)))
  
(defn match-route
 "Returns a vector (possibly empty) of matched segments or nil if the route doesn't match." 
 [segments route]
  (loop [r [] segments segments route route]
    (if-let [x (first route)]
      (cond
        (string? x) (when (= x (first segments)) 
                      (recur r (rest segments) (rest route)))
        (regex? x) (when (re-matches x (first segments))
                      (recur r (rest segments) (rest route)))
        (= '& x) (conj r segments)
        :else (when-let [segment (first segments)]
                (recur (conj r segment) (rest segments) (rest route))))
      (when-not (seq segments) r))))

(defn- extract-args [route]
  (let [params (remove simple-segment? route)
        simple-route (map #(if (simple-segment? %) % '_) route)
        params+alias (map #(if (vector? %) (conj % (gensym)) %) params)  
        args (map #(if (vector? %) (% 2) %) params+alias)
        validators (for [p params+alias :when (vector? p) :let [[v f alias] p]]
                     [v (cond
                          (regex? f) (list `re-matches f alias)
                          (string? f) `(when (= ~f ~alias) ~f)
                          :else (list f alias))])]
    [simple-route (vec args) validators]))

(defn- compile-handler-shorthand [form]
  (cond
    (vector? form) `(app ~@form)
    (map? form) `(app ~@(apply concat form)) 
    :else `(app ~form)))
                  
(defn- compile-route [segments [route form]]
  (let [handler (compile-handler-shorthand form) 
        etc-sym (gensym "etc")
        [fixed-route tail-binding] (split-route route etc-sym)
        [simple-fixed-route args validators] (extract-args fixed-route)
        simple-route (if tail-binding 
                       (concat simple-fixed-route ['&]) 
                       simple-fixed-route)  
        args (if tail-binding (conj args tail-binding) args)
        handler (if (= tail-binding etc-sym) 
                  `(alter-request ~handler assoc :uri (uri ~etc-sym)) 
                  handler)
        emit-validator (fn [body validator] `(when-let ~validator ~body))]
    `(when-let [~args (match-route ~segments '~simple-route)]
       ~(reduce emit-validator handler (reverse validators))))) 

(defn- compile-router [forms]
  (let [segments (gensym "segments")
        req (gensym "req")
        routes+forms (partition 2 forms)
        default-form ((apply array-map forms) '[&])
        routes+forms (if default-form
                       routes+forms
                       (concat routes+forms [['[&] `not-found]]))
        emit-match (fn [route+form]
                     `(when-let [handler# ~(compile-route segments route+form)]
                        (handler# ~req)))] 
    `(fn [~req] 
       (let [~segments (uri-segments ~req)]
         (or ~@(map emit-match routes+forms))))))

(defn- method-not-allowed-form [allowed-methods]
  (let [allow (apply str (interpose ", " (map #(.toUpperCase (name %) java.util.Locale/ENGLISH) allowed-methods)))]
    `(constantly {:status 405 :headers {"Allow" ~allow}})))  

(defn- compile-method-dispatch [spec]
  (let [spec (apply hash-map spec)
        else-form (:any spec)
        spec (dissoc spec :any)
        else-form (or else-form (method-not-allowed-form (keys spec)))] 
    `(fn [req#]
       ((condp = (:request-method req#)
          ~@(mapcat (fn [[k v]] [k (compile-handler-shorthand v)]) spec)
          ~else-form) req#))))

(defn- compile-text [s]
  `(fn [_#] {:status 200 :headers {"Content-Type" "text/plain;charset=UTF-8"} :body (str ~@s)}))
        
(defn compile-response-map [m]
  `(fn [_#] ~m))
        
(defmacro app
 "The main form."
 [& forms]
  (let [[middlewares etc] (split-with #(or (seq? %) (symbol? %)) forms)
        middlewares (reverse middlewares)
        [middlewares etc] 
          (if (seq etc) 
            [middlewares etc]
            [(rest middlewares) (list (first middlewares))])
        [params-map etc] (let [[x & xs] etc]
                           (if (and xs (map? x))
                             [x xs]
                             [nil etc]))
        handler (let [x (first etc)]
                  (cond
	                  (string? x) (compile-text etc)
	                  (vector? x) (compile-router etc)
	                  (keyword? x) (compile-method-dispatch etc)
	                  (map? x) (compile-response-map x)
	                  :else x))
        handler (if params-map 
                  `(fn self# [request#] 
                     (if-let [params# (:params request#)]
                       (let [~params-map params#]
                         (~handler request#))
                       ((-> self# kwp/wrap-keyword-params p/wrap-params) request#)))
                  handler)]
    (if (seq middlewares)
      `(-> ~handler ~@middlewares)
      handler)))

(defn delegate
 "Take a function and all the normal arguments to f but the first, and returns
  a 1-argument fn."
 [f & args]
  #(apply f % args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(comment
 ; an app can simply wrap a handler
 (app
   (fn [req] {:status 200 :headers {"Content-Type" "text/html"}
              :body "<h3>Hello World</h3>"}))
        
 ; an app can declare routes
 (app
   ["hello" name]
     (fn [req] {:status 200 :headers {"Content-Type" "text/html"}
                :body (str "<h3>Hello " name "</h3>")}))
                
 ; routes can be nested:
 (app
   ["hello" &]
     (app
       [name]
         (fn [req] {:status 200 :headers {"Content-Type" "text/html"}
                  :body (str "<h3>Hello " name "</h3>")})))
 
 ; params can be parsed/validated
 (app
   ["by-date" [[year month day] parse-date]] ...)
 
 ; and hence composed:
 (def by-name
   (app
     [name]
       (fn [req] {:status 200 :headers {"Content-Type" "text/html"}
                  :body (str "<h3>Hello " name "</h3>")})))
 
 (app
   ["hello" &] by-name
   ["greet" &] by-name)
 
 ;an app knows about methods: (without any, you get a 405 method Not Allowed)
  (app
    :get (fn [req] {:status 200 :headers {"Content-Type" "text/html"}
                  :body (str "GET!")})
    :post (fn [req] {:status 200 :headers {"Content-Type" "text/html"}
                  :body (str "POST!")}
    :any (fn [req] {:status 200 :headers {"Content-Type" "text/html"}
                  :body (str "ANYTHING ELSE!")}))
 
 
 ;an app can use existing middlewares (eg from compojure)
  (app
    with-multipart
    (with-session :memory)
    ["account" &]
      (app
        [] {:get (show-account (session :user))}
        ["upload-avatar"] {:post (save-upload-avatar (params :avatar-upload))})))  
    
)    
    
    
    
    
  