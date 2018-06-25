grasp=node js/grasp.js

build:
	PATH='node_modules/.bin:${PATH}' node_modules/.bin/pulp build

%.dot: %.grasp
	cat $< | ${grasp} > $@

%.png: %.dot
	dot -Kdot -Tpng $< > $@

cleanex:
	rm -f ${input}.dot ${input}.png

cleanps:
	rm -rf .pulp-cache output

supercleanps:
	rm -rf bower-components .pulp-cache output

# prevent make from deleting intermediate files
.PRECIOUS: ${input}.dot
