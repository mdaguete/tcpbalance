%%%-------------------------------------------------------------------------
%%% File     : tcp_proxy.erl
%%% Purpose  : A simple load balancing TCP proxy server
%%% Author   : Scott Lystig Fritchie, email: lhs=slfritchie, rhs=snookles.com
%%% Copyright: (c) 2003 Caspian Networks, Inc.
%%%-------------------------------------------------------------------------

-module(tcp_proxy).

-include("balance.hrl").

%% Setsockopt buffer size.
-define(BUFSIZ, (128*1024)).

%% External exports
-export([init/1, init/2]).

%% Internal exports
-export([accept_init/2, proxy/1]).

init(LocalPort) ->
    init(LocalPort, self()).
init(LocalPort, BalancerPid) ->
    spawn_link(?MODULE, accept_init, [LocalPort, BalancerPid]).

-record(state, {
	  timeout,
	  host,
	  port
	 }).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%
%%% accept_init() -- Runs in a process created by init(): open the TCP port
%%%    to listen on, then call acceptor()
%%%
%%% acceptor() -- Accepts new TCP connections on the listening socket, 
%%%    spawns a new proxy process (implemented by the proxy() function),
%%%    sets socket options on the new socket, transfers control of the 
%%%    socket to the new proxy proc, then sends a {go_for_it, ...} tuple
%%%    to signal the proxy proc that it can now continue execution.
%%%

accept_init(LPort, BalancerPid) ->
    {ok, LSock} = gen_tcp:listen(LPort, [binary, {backlog, 256},
					 {nodelay, true}, {reuseaddr, true},
					 {active, false}]),
    acceptor(LSock, BalancerPid).

acceptor(LSock, BalancerPid) ->
    case gen_tcp:accept(LSock) of
	{ok, CSock} ->
	    Proxy = spawn_link(?MODULE, proxy, [CSock]),
	    inet:setopts(CSock, [{recbuf, ?BUFSIZ}, {sndbuf, ?BUFSIZ}]),
	    gen_tcp:controlling_process(CSock, Proxy),
	    Proxy ! {go_for_it, CSock, BalancerPid, self()},
	    acceptor(LSock, BalancerPid);
	Error ->
	    exit(Error)
    end.

%%%
%%% proxy() -- Wait for the {go_for_it, ...} tuple from our parent that
%%%    tells us that it's OK to continue to execute proxy2().
%%%
%%%

proxy(CSock) ->
    receive
	{go_for_it, CSock, BalancerPid, AcceptorPid} -> 
	    link(BalancerPid),
	    unlink(AcceptorPid),
	    ok
    end,
    proxy2(CSock, BalancerPid, bal_proxy:get_be(BalancerPid)).

%%%
%%% proxy2() -- Figure out which back-end host to connect to.  If we
%%%    get a good connection, then proceed with proxy_loop().  If not,
%%%    inform the balancer that we got an error, then ask the balancer
%%%    for a new back-end.
%%%

proxy2(CSock, BalancerPid, {ok, RHost, RPort, ConnTimeout, ActTimeout}) ->
    case gen_tcp:connect(RHost, RPort, [binary, {nodelay, true},
					{active, true}, {recbuf, ?BUFSIZ},
				        {sndbuf, ?BUFSIZ}], ConnTimeout) of
	{ok, SSock} ->
	    bal_proxy:remote_ok(BalancerPid),
	    inet:setopts(CSock, [{active, true}]),
	    proxy_loop(CSock, SSock, #state{timeout = ActTimeout,
					    host = RHost, port = RPort});
	Error ->
	    error_logger:format("~s:proxy: TCP socket to remote server failed, Error = ~w\n", [?MODULE, Error]),
	    bal_proxy:remote_error(BalancerPid, Error),
	    proxy2(CSock, BalancerPid, bal_proxy:get_be(BalancerPid))
    end;
proxy2(CSock, BalancerPid, ?TIMEOUT_BE) ->
    error_logger:format("~s:proxy: backend timeout, no backends available at this time\n", [?MODULE]),
    exit(normal);
proxy2(CSock, BalancerPid, Answer) ->
    error_logger:format("~s:proxy: got bad answer = ~w\n", [?MODULE, Answer]),
    exit(bad_answer).

%%%
%%% proxy_loop(ClientSock, ServerSock, State)
%%%
%%% At the moment, State is just the activity timeout
%%%
%%% Note that, at the moment, we're a bit stupid by exiting whenever
%%% either socket is closed or has an error.  At least in the case of
%%% closure, a nice proxy would continue forwarding data in the other
%%% direction.  However, that would mean we would have to carry more
%%% state in State, horror!  :-)
%%%

proxy_loop(closed, closed, State) ->
    exit(byebye);
proxy_loop(CSock, SSock, State) ->
    receive
	{tcp, CSock, Data} ->
	    gen_tcp:send(SSock, Data),
	    proxy_loop(CSock, SSock, State);
	{tcp, SSock, Data} ->
	    gen_tcp:send(CSock, Data),
	    proxy_loop(CSock, SSock, State);
	{tcp_closed, Sock} ->
	    exit(byebye);
	{tcp_error, Sock} ->
	    error_logger:format("~s:proxy_loop: socket ~w ERROR\n", [?MODULE, Sock]),
	    exit(byebye);
	Msg ->
	    error_logger:format("~s:proxy_loop: got ~w\n", [?MODULE, Msg]),
	    proxy_loop(CSock, SSock, State)
    after State#state.timeout ->
	    error_logger:format("~s:proxy_loop: TIMEOUT after ~w\n", [?MODULE, State]),
	    exit(byebye)
    end.

