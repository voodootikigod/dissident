%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(webmachine_resource).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_server).
-export([behaviour_info/1]).
-export([start_link/1, start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([do/2]).
-define(TIMEOUT, 60000). %% one minute is fairly arbitrary... oh well.
-record(state, {mod, modstate, modexports}).

behaviour_info(callbacks) -> [{ping, 2}].

default(ping) ->
    no_default;
default(service_available) ->
    true;
default(resource_exists) ->
    true;
default(auth_required) ->
    true;
default(is_authorized) ->
    true;
default(forbidden) ->
    false;
default(allow_missing_post) ->
    false;
default(malformed_request) ->
    false;
default(uri_too_long) ->
    false;
default(known_content_type) ->
    true;
default(valid_content_headers) ->
    true;
default(valid_entity_length) ->
    true;
default(options) ->
    [];
default(allowed_methods) ->
    ['GET', 'HEAD'];
default(known_methods) ->
    ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT', 'OPTIONS'];
default(content_types_provided) ->
    [{"text/html", to_html}];
default(content_types_accepted) ->
    [];
default(delete_resource) ->
    false;
default(delete_completed) ->
    true;
default(post_is_create) ->
    false;
default(create_path) ->
    undefined;
default(process_post) ->
    false;
default(language_available) ->
    true;
default(charset_available) ->
    true;
default(encoding_available) ->
    true;
default(is_conflict) ->
    false;
default(multiple_choices) ->
    false;
default(previously_existed) ->
    false;
default(moved_permanently) ->
    false;
default(moved_temporarily) ->
    false;
default(last_modified) ->
    undefined;
default(expires) ->
    undefined;
default(generate_etag) ->
    undefined;
default(finish_request) ->
    true;
default(_) ->
    no_default.
          
start_link(Mod) ->
    start_link(Mod, []).

start_link(Mod, Args) ->
    gen_server:start_link(?MODULE, [Mod, Args], []).

%% @private
init([Mod, Args]) ->
    case Mod:init(Args) of
	{ok, State} ->
	    {ok, #state{mod=Mod, modstate=State, 
			modexports=dict:from_list(Mod:module_info(exports))}};
	_ ->
	    {stop, bad_init_arg}
    end.

do(Fun, ReqProps) when is_atom(Fun) andalso is_list(ReqProps) ->
    gen_server:call(proplists:get_value(pid, ReqProps), 
		    {do, Fun, ReqProps}, ?TIMEOUT).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% @private
handle_call({do, Fun, ReqProps}, _From, 
	    State=#state{modstate=ModState, mod=Mod}) ->
    {Reply, NewState} = handle_wm_call({Fun, ReqProps}, Mod, ModState, State),
    {reply, Reply, State#state{modstate=NewState}}.

%% @private
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_wm_call({Fun, ReqProps}, Mod, ModState, State) ->
    A = [ReqProps, ModState],
    case default(Fun) of
        no_default ->
            resource_call(Mod, Fun, A, ModState);
        Default ->
            case dict:is_key(Fun, State#state.modexports) of
                true ->
                    resource_call(Mod, Fun, A, ModState);
                false ->
                    {Default, ModState}
            end
    end.

resource_call(M, F, A, ModState) ->
    try
        apply(M, F, A)
    catch C:R ->
	    Reason = {C, R, erlang:get_stacktrace()},
            {{error, Reason}, ModState}
    end.

