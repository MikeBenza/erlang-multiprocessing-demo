%%%-------------------------------------------------------------------
%%% @author mbenza
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2018 9:54 PM
%%%-------------------------------------------------------------------
-module(child).
-author("mbenza").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    id :: non_neg_integer(),
    destinations :: list(pid()),
    cycles_remaining :: non_neg_integer(),
    messages_received = 0 :: non_neg_integer(),
    settings :: #{
        check_up_cycles := non_neg_integer(),
        n_destinations := non_neg_integer()
    }
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(Id :: non_neg_integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Id) ->
    gen_server:start_link({local, binary_to_atom(list_to_binary(io_lib:format("~p_~p", [?SERVER, Id])), utf8)}, ?MODULE, [Id], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init([Id ::non_neg_integer()]) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Id]) ->
    InitialDelay = trunc(rand:uniform() * 1000),
    gen_server:cast(self(), get_destinations),
    erlang:send_after(InitialDelay, self(), chat),
    Cycles  = trunc(rand:uniform() * 45) + 5,
    Settings = #{
        check_up_cycles => Cycles,
        n_destinations => 5
    },
    {ok, #state{id = Id, settings = Settings, cycles_remaining = Cycles}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, _From, State) ->
    {reply, {error, {invalid_message, Request}}, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(get_destinations, #state{settings = #{n_destinations := NDestinations}} = State) ->
    {ok, Destinations} = process_manager:get_n_siblings(NDestinations),
    {noreply, State#state{destinations = Destinations}};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(chat, #state{destinations = Destinations, messages_received = Received} = State) ->
    erlang:send_after(1000, self(), chat),
    lists:foreach(fun send_message/1, Destinations),
    report_stats(Received, length(Destinations)),
    NewState = update_state(State),
    {noreply, NewState};
handle_info({mail, #{from := _From, message := _Ref}}, #state{messages_received = Received} = State) ->
%    io:format("~p Received message ~p from ~p~n", [self(), Ref, From]),
    {noreply, State#state{messages_received = Received + 1}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_state(#state{cycles_remaining = 1}) ->
    Settings = #{check_up_cycles := CheckUpCycles, n_destinations := NDestinations} = process_manager:get_settings(),
    {ok, Destinations} = process_manager:get_n_siblings(NDestinations),
    #state{cycles_remaining = CheckUpCycles, destinations = Destinations, settings = Settings, messages_received = 0};
update_state(#state{cycles_remaining = CyclesRemaining} = State) ->
    State#state{cycles_remaining = CyclesRemaining - 1, messages_received = 0}.

send_message(Destination) ->
    %Ref = make_ref(),
%    io:format("~p Sending message to ~p.  Message: ~p~n", [self(), Destination, hello]),
    Destination ! {mail, #{from => self(), message => hello}}.

report_stats(Received, Sent) ->
    announcer:report(self(), Received, Sent).
