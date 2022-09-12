# Compile Elm in debug mode
develop:
	elm make src/Main.elm --output=../static/cars/js/cars2.js --debug

# Compile Elm for production (with optimize flag)
compile:
	elm make src/Main.elm --output=../static/cars/js/cars2.js --optimize

# Uglify js output file
uglify:
	uglifyjs ../static/cars/js/cars2.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output ../static/cars/js/cars2.js

release: compile uglify