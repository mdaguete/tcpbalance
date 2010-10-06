%%%-------------------------------------------------------------------------
%%% File     : balance.erl
%%% Purpose  : TCP load balancing application.
%%% Author   : Scott Lystig Fritchie, email: lhs=slf, rhs=caspiannetworks.com
%%% Copyright: (c) 2003 Caspian Networks, Inc.
%%%-------------------------------------------------------------------------

-module(balance).

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(application).

-include("balance.hrl").

%% application callbacks
-export([start/0, start/2, stop/1]).

%%%
%%% As far as I know, the Erlang application & release management parts
%%% of the OTP (Open Telecom Platform) don't allow you to run mulitple
%%% instances of the same application.  E.g. Running this application
%%% several times to provide HTTP, SMTP, and NNTP proxy services.
%%%
%%% It's possible to do, but you need to build some of your own
%%% application management infrastructure ... which is beyond the scope
%%% of this small hack.  Therefore, do not be surprised if you see 
%%% assumptions that there's only one balance app running at a time.
%%%
%%% See http://www.erlang.org/doc/r9b/doc/system.html, in particular
%%% the "Design Principles" document.
%%%

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start() ->
    %% io:format("XXX ~w:start/0\n", [?MODULE]),
    start(xxxwhocares, []).
start(Type, []) ->
    %% io:format("XXX ~w:start/2 []\n", [?MODULE]),
    start(xxxwhocares, [local_tcp_port(), be_conn_timeout(),
			be_inactivity_timeout()]);
start(Type, StartArgs) ->
    %% io:format("XXX ~w:start/2 Type = ~w, StartArgs = ~w\n", [?MODULE, Type, StartArgs]),
    %%
    %% Application environment values: fetch via application:get_all_env et al.
    %% Sources:
    %%    1. 'env' portion of ethbridge.app file
    %%    2. from a .config file via "-config filename" on command line
    %%    3. from "-ethbridge keyX valX keyY valY ..."
    %%
    bal_sup:start_link(StartArgs).    

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%----------------------------------------------------------------------
stop(State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

local_tcp_port() ->
    get_app_env(?BALANCER_APP, local_tcp_port, 3632).

be_conn_timeout() ->
    get_app_env(?BALANCER_APP, be_conn_timeout, 5*1000).

be_inactivity_timeout() ->
    get_app_env(?BALANCER_APP, be_inactivity_timeout, 180*1000).

get_app_env(App, Key, Default) ->
    case application:get_env(App, Key) of
	{ok, Val} -> Val;
	_         -> Default
    end.
