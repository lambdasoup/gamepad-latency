.PHONY: debug clean serve release

src = src/*.elm src/index.html

debug: $(src)
	mkdir -p public
	cp src/index.html public/
	elm make src/Main.elm --output public/elm.js --debug

clean:
	rm -rf public

release: $(src)
	mkdir -p public
	cp src/index.html public/
	elm make src/Main.elm --optimize --output public/elm.js

serve:
	while true; do \
		kill `cat .pid`; \
		clear ;\
		make debug; \
		cd public; python -m SimpleHTTPServer & echo $$! > .pid; cd ..;\
		inotifywait -qre close_write $(src); \
	done
