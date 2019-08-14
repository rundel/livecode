
all:
	rm -rf themes/ langs/ plugins/ prism/
	mkdir themes langs plugins
	git clone git@github.com:PrismJS/prism.git
	cp prism/LICENSE ./
	cp prism/components/*.min.js langs/
	cp prism/themes/*.css themes/
	cp prism/components/*.min.js langs/
	find prism/plugins/ -type f -name "*.css" -exec cp {} plugins/ \;
	find prism/plugins/ -type f -name "*.min.js" -exec cp {} plugins/ \;
	rm -rf prism/

.PHONY: all
