.PHONY: qa live test debug clean watch release serve

src = src/*
site = $(shell jq -r '.projects.default' .firebaserc)

debug: $(src)
	@mkdir -p public
	make public/index.html
	elm make src/Main.elm --output public/elm.js --debug

clean:
	rm -rf public

release: $(src)
	@mkdir -p public
	make public/index.html
	elm make src/Main.elm --optimize --output public/elm.js

watch:
	while true; do \
		clear ;\
		make debug --no-print-directory; \
		inotifywait -rqe create,delete,modify,move $(src); \
	done

public/index.html: src/data.json src/index.mustache
	mustache src/data.json src/index.mustache > public/index.html

serve:
	browser-sync public -w

test: release
	firebase emulators:start

qa: release
	firebase hosting:channel:deploy qa

live:
	firebase hosting:clone $(site):qa $(site):live
