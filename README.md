# gemtd

Gem td game in elm.



 # Compile the código

 ```
elm make src/Main.elm --output build/gemtd.js
 ```

Build optmimized
 ```
elm make src/Main.elm --output --optimize build/tetris.js
 ```

uglify
 ```
uglifyjs build/tetris.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output build/tetris.min.js
 ```

If uglify is not available
```
npm install uglify-js --global
``` 

# Open web on dev with visual studio
Live server over app.html 

# Añadir el juego a un documento html
  
  importar el fichero .js y .css y añadir las líneas del body.

```
  <script src="../build/gemtd.js"></script>
    <link rel="stylesheet" href="gemtd.css" />

</head>

<body>
    
    <div id="gemtd"></div>

    <script>
        var gemtd = Elm.Main.init({
          node: document.getElementById('gemtd')
        });
    </script>
```

# References

Información about gemtd, rules and mechanics

https://clementbera.github.io/Website/creeps.html


Guide of elm

https://guide.elm-lang.org/install/elm.html

https://package.elm-lang.org/packages/perty/matrix/latest/Matrix

More about optimizing build 

https://github.com/elm/compiler/blob/master/hints/optimize.md