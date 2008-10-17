ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := dissident

all: build_modules erl ebin/$(APP).app
	
build_modules:
	mkdir -p deps/mochiweb/ebin
	mkdir -p deps/webmachine/ebin
	(cd deps/mochiweb ;make) 
	(cd deps/webmachine ;make)

init_modules:
	git submodule init
	git submodule update

update_modules:
	git submodule update
	(cd deps/mochiweb ;git remote update) 
	(cd deps/webmachine ;git remote update)

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

