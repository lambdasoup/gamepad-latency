.PHONY: debug clean serve public

src = *.elm index.html
out = public/elm.js public/index.html

debug: $(src)
	mkdir -p public
	cp index.html public/
	elm make Main.elm --output public/elm.js --debug

clean:
	rm -rf public

public: $(src)
	mkdir -p public
	cp index.html public/
	elm make Main.elm --optimize --output public/elm.js

serve:
	while true; do \
		kill `cat .pid`; \
		clear ;\
		make debug; \
		cd public; python -m SimpleHTTPServer & echo $$! > .pid; cd ..;\
		inotifywait -qre close_write $(src); \
	done
