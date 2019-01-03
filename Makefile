grasp=node js/grasp.js

build:
	npm run build

testcli:
	npm run build
	cat example/test1.grasp | node cli/grasp.js
	cat example/test1.grasp | node cli/grasp.js -g
	cat example/test1.grasp | node cli/grasp.js -p

%.dot: %.grasp
	cat $< | ${grasp} > $@

%.png: %.dot
	dot -Kdot -Tpng $< > $@

cleanex:
	rm -f example/*.dot example/*.png

cleanps:
	rm -rf .pulp-cache output

supercleanps:
	rm -rf bower-components .pulp-cache output

# prevent make from deleting intermediate files
.PRECIOUS: example/test1.dot
