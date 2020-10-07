grasp=node cli/grasp.js

build:
	npm run build

testcli:
	npm run build
	cat example/test1.grasp | ${grasp}
	cat example/test1.grasp | ${grasp} -g
	cat example/test1.grasp | ${grasp} -p

%.dot: %.grasp
	cat $< | ${grasp} -g > $@

%.png: %.dot
	dot -Kdot -Tpng $< > $@

cleanex:
	rm -f example/*.dot example/*.png

cleanps:
	rm -rf output

supercleanps:
	rm -rf .spago output

# prevent make from deleting intermediate files
.PRECIOUS: example/test1.dot
