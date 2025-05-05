%%%-------------------------------------------------------------------
%% @doc irc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(irc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [#{id => "server", start => {irc_server, start_link, [{}]}}, #{id => "client_sup", start=> {irc_client_sup, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
setup() ->
	{ok, ListenSocket} = gen_tcp:listen(6667, [binary, {active, false}]),
	ListenSocket.
