(ns net.cgrand.ring.resource)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn uri-segments [{:keys [#^String uri]}]
  (rest (map #(java.net.URLDecoder/decode % "UTF-8") (.split uri "/"))))

(defn- prefix-route? [route]
  (= '& (peek route)))

(def pass (constantly nil))

(defn rebase-uri [segments handler]
  (fn [{:keys [uri] :as req}]
    (handler (assoc req :uri (apply str "/" (interpose "/" segments))))))

(defn match-route [segments route]
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
  (vec (remove #(or (string? %) (= '& %)) route)))

(defn- compile-handler [form]
  (cond 
    (vector? form) `(app ~@form) 
    (map? form) `(app ~@(apply concat form)) 
    :else form))
    
(defn- compile-route [segments [route form]]
  (let [route-body (compile-handler form) 
        args (extract-args route)
        etc-sym (when (prefix-route? route) (gensym "etc"))
        route-body (if etc-sym `(rebase-uri ~etc-sym ~route-body) route-body)
        args (if etc-sym (conj args etc-sym) args)]
    `(when-let [~args (match-route ~segments '~route)] ~route-body))) 

(defn- compile-routes [forms]
  (let [segments (gensym "segments")] 
    `(fn [req#] 
       (let [~segments (uri-segments req#)]
         ((or ~@(map #(compile-route segments %) (partition 2 forms))
            pass) req#)))))

(defn- compile-resource [spec]
  (let [else-form (:any spec `pass)
        spec (dissoc spec :any :deny)] 
    `(fn [req#]
       ((condp = (:request-method req#)
          ~@(mapcat (fn [[k v]] [k (compile-handler v)]) spec)
          ~else-form) req#))))
        
(defmacro app [& forms]
  (let [[middlewares etc] (split-with #(or (seq? %) (symbol? %)) forms)
        middlewares (reverse middlewares)
        [middlewares [x :as etc]] 
          (if (seq etc) 
            [middlewares etc]
            [(rest middlewares) (list (first middlewares))])
        middlewares (vec middlewares)
        handler (cond
                  (vector? x) (compile-routes etc)
                  (keyword? x) (compile-resource (apply hash-map etc))
                  :else x)]
    (if (seq middlewares)
      `(-> ~handler ~middlewares)
      handler))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(comment
 ; an app can simply wrap a handler
 (app 
   (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
              :body "<h3>Hello World from Ring</h3>"}))
        
 ; an app can declare routes
 (app 
   ["hello" name] 
     (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
                :body (str "<h3>Hello " name " from Ring</h3>")}))
                
 ; routes can be nested:
 (app 
   ["hello" &]
     (app
       [name] 
         (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
                  :body (str "<h3>Hello " name " from Ring</h3>")})))

 ; or:
 (app 
   ["hello" &]
     [[name] 
        (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
                   :body (str "<h3>Hello " name " from Ring</h3>")})])
 
 
 ;an app knows about methods:
  (app
    :get (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
                  :body (str "GET!")})
    :post (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
                  :body (str "POST!")}))

 ;an app knows about methods:
  (app
    :get (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
                  :body (str "GET!")})
    :any (fn [req] {:status 200 :headers {"Content-Type" "text/html"}   
                  :body (str "NOT GET!")}))
)  
    
    
    
    
    
    
  