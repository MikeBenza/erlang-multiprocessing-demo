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
    total_received = 0 :: non_neg_integer(),
    total_sent = 0 :: non_neg_integer(),
    last_n = {[], []} :: {list(non_neg_integer()), list(non_neg_integer())}
}).

-record(record_entry, {
    who      = undefined :: pid(),
    received = 0         :: non_neg_integer(),
    sent     = 0         :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

report(Who, Received, Sent) ->
    ets:insert(?SENT_RECEIVED_TABLE, #record_entry{who = Who, received = Received, sent = Sent}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Note: 99.9% of the time when creating an ETS table that you plan to put records in, you specify {keypos, ...} as
    % one of the options.  That indicates which position in the tuple is the key for the items in the table.  That is
    % done here.  This ETS table is a duplicate_bag, meaning it's just a bunch of entries which may have the same key
    % and there may be duplicates of entries.  If the keypos is not specified it defaults to 1 (the first entry in the
    % tuple).  The record that gets inserted is a record_entry (search this file for that).  The record syntax is
    % actually just syntactical sugar and the record #record_entry{who=A, received=B, sent=C} just gets converted to
    % {record_entry, who, received, sent}.
    %
    % With the keypos = 1, then each item in the ETS table will have the key 'record_entry'.  This is intentional.  This
    % allows ets:take/2 to get and delete all of the entries in the table in a single atomic call, ensuring that every
    % single message that was sent or received is accounted for.
    ets:new(?SENT_RECEIVED_TABLE, [named_table, duplicate_bag, public, {write_concurrency, true}]),
    timer:send_interval(?ANNOUNCE_DELAY, announce),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(announce, #state{total_received = TotalReceived,
                             total_sent = TotalSent,
                             last_n = {LastNSent, LastNReceived}} = State) ->
    % Use ets:take/2 to get all of the records in the SENT_RECEIVED_TABLE.  Sum them up in a single fold.
    {Received, Sent} = lists:foldl(
        fun (#record_entry{sent = RecordSent, received = RecordReceived}, {R, S}) ->
            {R + RecordReceived, S + RecordSent}
        end,
        {0, 0},
        ets:take(?SENT_RECEIVED_TABLE, record_entry)
    ),

    NewTotalReceived = TotalReceived + Received,
    NewTotalSent = TotalSent + Sent,

    NewLastSent = append_with_max_length(Sent, LastNSent, ?MOVING_AVERAGE_QUANTITY),
    NewLastReceived = append_with_max_length(Received, LastNReceived, ?MOVING_AVERAGE_QUANTITY),

    SentMovingAverage = lists:sum(NewLastSent) / min(?MOVING_AVERAGE_QUANTITY, length(NewLastSent)),
    ReceivedMovingAverage = lists:sum(NewLastReceived) / min(?MOVING_AVERAGE_QUANTITY, length(NewLastReceived)),

    if
        Sent + Received > 0 ->
            io:format("Recv'd: ~p, sent: ~p.  MovAvg(~p): Recv'd: ~p, sent: ~p.  Total recv'd: ~p, sent: ~p.~n",
                [Received, Sent, ?MOVING_AVERAGE_QUANTITY, ReceivedMovingAverage, SentMovingAverage, NewTotalReceived, NewTotalSent]);
        true ->
            ok
    end,

    {noreply, State#state{total_received = NewTotalReceived, total_sent = NewTotalSent, last_n = {NewLastSent, NewLastReceived}}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

append_with_max_length(New, Previous, MaxLength) when is_list(Previous), is_integer(MaxLength) ->
    [New | lists:sublist(Previous, MaxLength - 1)].
