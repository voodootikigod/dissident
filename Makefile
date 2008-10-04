ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := dissident

all: erl ebin/$(APP).app
	mkdir -p deps/mochiweb/ebin
	mkdir -p deps/webmachine/ebin
	(cd deps/mochiweb ;make) 
	(cd deps/webmachine ;make)
	

modules:
	git submodule init
	git submodule update

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app deps/*/ebin/*.app deps/*/ebin/*.beam

ebin/$(APP).app:
	@cp -v src/$(APP).app $@

