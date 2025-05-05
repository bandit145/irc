-module(irc_server).
-behavior(gen_server).

-export([handle_cast/2,handle_call/3,terminate/2, handle_info/2, start_link/1, init/1]).

-record(server_state, {active_sessions, socket}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(_) ->
	{ok, ListenSocket} = gen_tcp:listen(6667, [binary,{active, true}]),
	gen_server:cast(self(), tick),
	{ok, #server_state{active_sessions=#{}, socket=ListenSocket}}.


handle_cast(tick, State) ->
	{ok, Socket} = gen_tcp:accept(State#server_state.socket),
	ClientSup = whereis(irc_client_sup),
	{ok, {Address, Port}} = inet:peername(Socket),
	io:format("~p", [Port]),
	{ok, Pid} = supervisor:start_child(ClientSup, #{id => {Address, Port}, start => {irc_client, start_link, [{Socket, {Address, Port}}]}, restart => transient}),
	gen_tcp:controlling_process(Socket, Pid),
	gen_server:cast(self(), tick),
	{noreply, State};

handle_cast(_,State) ->
	{noreply, State}.

handle_call(_,_,State) ->
	{noreply, State}.

handle_info(Info, State) ->
	io:format("~p", [Info]),
	{noreply, State}.
	
terminate(_,#server_state{socket=Socket}) ->
	ok = gen_tcp:close(Socket),
	ok.
