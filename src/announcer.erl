%%%-------------------------------------------------------------------
%%% @author mbenza
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2018 9:56 PM
%%%-------------------------------------------------------------------
-module(announcer).
-author("mbenza").

-behaviour(gen_server).

-define(ANNOUNCE_DELAY, 1000).
-define(MOVING_AVERAGE_QUANTITY, 10).

%% API
-export([start_link/0, report/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(SENT_RECEIVED_TABLE, sent_received_table).

-record(state, {
    previous_received = 0 :: non_neg_integer(),
    previous_sent = 0 :: non_neg_integer(),
    last_n = {[], []} :: {list(non_neg_integer()), list(non_neg_integer())}
}).

%%%===================================================================
%%% API
%%%===================================================================

report(_Who, Received, Sent) ->
    ets:update_counter(?SENT_RECEIVED_TABLE, received, Received, {received, 0}),
    ets:update_counter(?SENT_RECEIVED_TABLE, sent, Sent, {sent, 0}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?SENT_RECEIVED_TABLE, [named_table, set, public, {write_concurrency, true}]),
    timer:send_interval(?ANNOUNCE_DELAY, announce),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(announce, #state{previous_received = PreviousReceived,
                             previous_sent = PreviousSent,
                             last_n = {LastNSent, LastNReceived}} = State) ->

    {LatestReceived, LatestSent} = try
        [{received, InnerLatestReceived}, {sent, InnerLatestSent}] = lists:sort(ets:tab2list(?SENT_RECEIVED_TABLE)),
        {InnerLatestReceived, InnerLatestSent}
    catch
        _:_  ->
            {0, 0}
    end,

    Received = LatestReceived - PreviousReceived,
    Sent = LatestSent - PreviousSent,

    NewLastSent = append_with_max_length(Sent, LastNSent, ?MOVING_AVERAGE_QUANTITY),
    NewLastReceived = append_with_max_length(Received, LastNReceived, ?MOVING_AVERAGE_QUANTITY),

    SentMovingAverage = lists:sum(NewLastSent) / min(?MOVING_AVERAGE_QUANTITY, length(NewLastSent)),
    ReceivedMovingAverage = lists:sum(NewLastReceived) / min(?MOVING_AVERAGE_QUANTITY, length(NewLastReceived)),

    if
        Sent + Received > 0 ->
            io:format("Recv'd: ~p, sent: ~p.  MovAvg(~p): Recv'd: ~p, sent: ~p.  Total recv'd: ~p, sent: ~p.~n",
                [Received, Sent, ?MOVING_AVERAGE_QUANTITY, ReceivedMovingAverage, SentMovingAverage, LatestReceived, LatestSent]);
        true ->
            ok
    end,

    {noreply, State#state{previous_received = LatestReceived, previous_sent = LatestSent, last_n = {NewLastSent, NewLastReceived}}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

append_with_max_length(New, Previous, MaxLength) when is_list(Previous), is_integer(MaxLength) ->
    [New | lists:sublist(Previous, MaxLength - 1)].
