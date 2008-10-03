ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := webmachine

all: erl ebin/$(APP).app
	(cd deps/mochiweb ;make) 
	

submodules:
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
