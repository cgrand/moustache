(ns net.cgrand.ring.resource)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn uri-segments
 "Splits the uri of the given request map around / and decode segments." 
 [{:keys [#^String uri]}]
  (rest (map #(java.net.URLDecoder/decode % "UTF-8") (.split uri "/" -1))))
  
(defn uri
 "Turns a seq of decoded segment into an uri."  
 [segments]
  (apply str "/" (interpose "/" (map #(java.net.URLEncoder/encode % "UTF-8") segments))))

(defn- prefix-route? [route]
  (= '& (peek route)))

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

(defn match-route
 "Returns a vector (possibly empty) of matched segments or nil if the route doesn't match." 
 [segments route]
  (loop [r [] segments segments route route]
    (if-let [x (first route)]
      (cond
        (string? x) (when (= x (first segments)) 
                      (recur r (rest segments) (rest route)))
        (= '& x) (conj r segments)
        :else (when-let [segment (first segments)]
                (recur (conj r segment) (rest segments) (rest route))))
      (when-not (seq segments) r))))
      
(defn- extract-args [route]
  (let [params (remove #(or (string? %) (= '& %)) route)
        simple-route (map #(if (or (string? %) (= '& %)) % '_) route)
        params+alias (map #(if (vector? %) (conj % (gensym)) %) params)  
        args (map #(if (vector? %) (% 2) %) params+alias)
        bindings (for [p params+alias :when (vector? p) :let [[v f alias] p]]
                   [v (if (instance? java.util.regex.Pattern f)
                        (list `re-matches f alias)
                        (list f alias))])]
    [simple-route (vec args) bindings]))

(defn- compile-handler-shorthand [form]
  (cond
    (vector? form) `(app ~@form)
    (map? form) `(app ~@(apply concat form)) 
    :else `(app ~form)))
                  
(defn- compile-route [segments [route form]]
  (let [handler (compile-handler-shorthand form) 
        [simple-route args bindings] (extract-args route)
        etc-sym (when (prefix-route? route) (gensym "etc"))
        handler (if etc-sym `(alter-request ~handler assoc :uri (uri ~etc-sym)) handler)
        args (if etc-sym (conj args etc-sym) args)
        emit-bindings 
          (fn emit-bindings [bindings]
            (if-let [[binding & etc] (seq bindings)]
              `(when-let ~binding
                 ~(emit-bindings etc))
              handler))]
    `(when-let [~args (match-route ~segments '~simple-route)]
       ~(emit-bindings bindings)))) 

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
  `(constantly {:status 200 :headers {"Content-Type" "text/plain;charset=UTF-8"} :body (str ~@s)}))
        
(defmacro app
 "The main form."
 [& forms]
  (let [[middlewares etc] (split-with #(or (seq? %) (symbol? %)) forms)
        middlewares (reverse middlewares)
        [middlewares [x :as etc]] 
          (if (seq etc) 
            [middlewares etc]
            [(rest middlewares) (list (first middlewares))])
        middlewares (vec middlewares)
        handler (cond
                  (string? x) (compile-text etc)
                  (vector? x) (compile-router etc)
                  (keyword? x) (compile-method-dispatch etc)
                  :else x)]
    (if (seq middlewares)
      `(-> ~handler ~middlewares)
      handler))) 

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
    
    
    
    
  