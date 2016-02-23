build:
	elm make Main.elm --output=elm.js

watch:
	-$(MAKE) build
	fswatch *.elm | \
	while read f; do \
	    echo "======================================================================="; \
	    echo; echo; echo $$f; \
	    $(MAKE) build; \
	done

browse:
	browser-sync start --server --startPath people.html --files elm.js

