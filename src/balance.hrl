%%%-------------------------------------------------------------------------
%%% File     : balance.hrl
%%% Purpose  : TCP load balancing application.
%%% Author   : Scott Lystig Fritchie, email: lhs=slf, rhs=caspiannetworks.com
%%% Copyright: (c) 2003 Caspian Networks, Inc.
%%%-------------------------------------------------------------------------

%% Name of the balancer application
-define(BALANCER_APP, balance).

%% Module name for the TCP proxy
-define(TCPPROXY, tcp_proxy).

%% Atom used to inform tcp_proxy proc that no backends are available.
-define(TIMEOUT_BE, timeout_be).
