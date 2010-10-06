%%%-------------------------------------------------------------------------
%%% File     : bal_sup.erl
%%% Purpose  : Top-level supervisor for TCP load balancing proxy.
%%% Author   : Scott Lystig Fritchie, email: lhs=slf, rhs=caspiannetworks.com
%%% Copyright: (c) 2003 Caspian Networks, Inc.
%%%-------------------------------------------------------------------------

-module(bal_sup).
-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    % io:format("XXX ~w:start_link()\n", [?MODULE]),
    {error, "Must include StartArgs list"}.
start_link(StartArgs) ->
    % io:format("XXX ~w:start_link ArgList = ~w\n", [?MODULE, StartArgs]),
    supervisor:start_link({local, balance_sup}, ?MODULE, StartArgs).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%
%% The cd_sup supervisor will use a one_for_all strategy: if a child dies,
%% all remaining children are killed before restarting the children.
%%----------------------------------------------------------------------

init(ArgList) ->
    % io:format("XXX ~w:init ArgList = ~w\n", [?MODULE, ArgList]),
    %% Child_spec = {IdName, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used}
    BalProxy = {bal_proxy, {bal_proxy, start_link, [ArgList]},
                     permanent, 2000, worker, [bal_proxy, tcp_proxy]},
    %%WebServer = {httpd, {httpd, start_service, ["../priv/inets.conf"]},
    %%	 permanent, 2000, worker, [httpd]},
    %% {one_for_all, MaxRestarts, MaxTimeInSeconds}, where 'one_for_all' is the
    %% supervisor's restart strategy.  See "supervisor" docs, CALLBACK
    %% FUNCTIONS section, Module::init/1 func.
    %% 5 times in 60 seconds is not very realistic for real-world
    %% deployment, but it's alright for "using appmon to kill random
    %% processes" demonstration purposes.
    {ok, {{one_for_all, 5, 60}, [
                                   BalProxy
				   %WebServer
				  ]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% None.
