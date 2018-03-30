JBUILDER := jbuilder

.PHONY: lib clean

lib:
	$(JBUILDER) build

clean:
	$(JBUILDER) clean