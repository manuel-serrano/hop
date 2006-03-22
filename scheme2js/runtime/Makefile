all: runtime_mapping.alist runtime-interface.js

exporter: exporter.scm
	bigloo -o $@ $^

runtime_mapping.alist: runtime.js exporter
	./exporter $< mapping > $@

runtime-interface.js: runtime.js exporter
	./exporter $< interface > $@

clean:
	rm -f exporter runtime_mapping.alist runtime-interface.js *.o
