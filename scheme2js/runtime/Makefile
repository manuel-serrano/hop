all: runtime_mapping.sch runtime-interface.js

exporter: exporter.scm
	bigloo -o $@ $^

runtime_mapping.sch: runtime.js exporter
	./exporter $< mapping > $@

runtime-interface.js: runtime.js exporter
	./exporter $< interface > $@

clean:
	rm -f exporter;
	rm -f *.o;
	rm -f runtime_mapping.sch runtime-interface.js
