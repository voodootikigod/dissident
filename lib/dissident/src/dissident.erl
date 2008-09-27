%%%----------------------------------------------------------------
%%% @author Chris Williams <chris@iterativedesigns.com>
%%% @doc
%%%		The Dissident server that will set up and establish 
%%%		JSON connections between the host and the backend.
%%% @end
%%% @copyright 2008 Iterative Designs
%%%----------------------------------------------------------------,
-module(dissident).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, register/2, unregister/1, worker/0, 
         call/4, call/5, cast/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% 
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
	
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


register(Name, Pid) ->
    gen_server:cast(?SERVER, {register, Name, Pid}).

unregister(Name) ->
    gen_server:cast(?SERVER, {unregister, Name}).

worker() ->
    gen_server:call(?SERVER, worker).

call(Pid, Name, Id, MessageString) ->
    gen_server:call(Pid, {message, Name, Id, MessageString}).

call(Pid, Name, Id, MessageString, Timeout) ->
    gen_server:call(Pid, {message, Name, Id, MessageString, Timeout}, 
                    Timeout).

cast(Pid, Name, Id, MessageString) ->
    gen_server:cast(Pid, {message, Name, Id, MessageString}).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}.
%% 
%% @doc 
%% Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call({message, Name, Id, MessageString}, _From, State) ->
    Reply = send_call(Name, Id, MessageString, State, none),
    {reply, Reply, State};
handle_call({message, Name, Id, MessageString, Timeout}, _From, State) ->
    Reply = send_call(Name, Id, MessageString, State, Timeout),
    {reply, Reply, State};
handle_call(worker, _From, State) ->
    Res = case trc_worker_sup:start_child(State) of
              {ok,Child} ->
                  Child;
              {ok, Child, _Info} ->
                  Child;
              Error ->
                  Error
          end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({register, Name, Pid}, State) ->
    register_internal(Name, Pid, State, []),
    {noreply, State};
handle_cast({unregister, Name}, State) ->
    unregister_internal(Name, State, []),
    {noreply, State};
handle_cast({message, Name, Id, MessageString}, State) ->
    send_cast(Name, Id, MessageString, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% 
%% @doc 
%% Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, noproc}, State) ->
    unregister_pid_internal(Pid, State, []);
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc 
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% 
%% @doc 
%% Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
register_internal(Name, Pid, [{Name, _} | T], Acc) ->
    [{Name, Pid} | Acc] ++ T;
register_internal(Name, Pid, [H | T], Acc) ->
    register_internal(Name, Pid, T, [H | Acc]);
register_internal(Name, Pid, [], Acc) ->
    [{Name, Pid} | Acc].


unregister_internal(Name, [{Name, _} | T], Acc) ->
    Acc ++ T;
unregister_internal(Name, [H | T], Acc) ->
    unregister_internal(Name, T, [ H | Acc ]);
unregister_internal(_Name, [], Acc) ->
    Acc.


unregister_pid_internal(Pid, [{_ , Pid} | T], Acc) ->
    Acc ++ T;
unregister_pid_internal(Pid, [H | T], Acc) ->
    unregister_pid_internal(Pid, T, [ H | Acc ]);
unregister_pid_internal(_Pid, [], Acc) ->
    Acc.


send_call(Name, Id, MessageString, State, Timeout) ->
    Message = tcomm_tuple:decode(MessageString),
    Result = case get_pid(Name, State) of
                 undefined ->
                     {error, {string, string:concat("No Server named ",
                                                    Name)}};
                 Pid ->
                     case Timeout of
                         none ->
                             gen_server:call(Pid, {tercio_msg, Id, Message});
                         _ ->
                             gen_server:call(Pid, {tercio_msg, 
                                                   Id, Message}, Timeout)
                     end
             end,
    tcomm_json:encode(Result).

send_cast(Name, Id, MessageString, State) ->
    Message = tcomm_tuple:decode(MessageString),
    case get_pid(Name, State) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {tercio_msg, Id, Message})
    end.



get_pid(Name, [{Name, Pid} | _]) -> 
    Pid;
get_pid(Name, [ _ | T ]) ->
    get_pid(Name, T);
get_pid(_Name, []) ->
    undefined.
