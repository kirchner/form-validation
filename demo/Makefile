build:
	mkdir -p ./gh-pages
	mkdir -p ./gh-pages/css
	cp static/index.html gh-pages/index.html
	cp static/css/* gh-pages/css
	elm make --output=gh-pages/elm.js  --optimize src/MainWithValidations.elm
	uglifyjs gh-pages/elm.js \
	    --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
	    | uglifyjs --mangle --output=gh-pages/elm.min.js
	mv gh-pages/elm.min.js gh-pages/elm.js
