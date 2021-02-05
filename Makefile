.PHONY: qa live test debug clean watch release serve

src = src/*.elm src/index.html
site = $(shell jq -r '.projects.default' .firebaserc)

debug: $(src)
	@mkdir -p public
	cp src/index.html public/
	elm make src/Main.elm --output public/elm.js --debug

clean:
	rm -rf public

release: $(src)
	@mkdir -p public
	cp src/index.html public/
	elm make src/Main.elm --optimize --output public/elm.js

watch:
	while true; do \
		clear ;\
		make debug --no-print-directory; \
		inotifywait $(src); \
	done

serve:
	browser-sync public -w

test: release
	firebase emulators:start

qa: release
	firebase hosting:channel:deploy qa

live:
	firebase hosting:clone $(site):qa $(site):live
