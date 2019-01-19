%%%-------------------------------------------------------------------
%%% @author mbenza
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2018 9:56 PM
%%%-------------------------------------------------------------------
-module(process_manager).
-author("mbenza").

-behaviour(gen_server).

%% API
-export([start_link/0, start_n_processes/1, get_n_siblings/1, get_settings/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(PROCESSES_TABLE, processes_table).

-record(state, {
    processes = [] :: list(),
    received = 0:: non_neg_integer(),
    sent = 0 :: non_neg_integer(),
    total_received = 0 :: non_neg_integer(),
    total_sent = 0 :: non_neg_integer()
}).

-record(proc_table_entry, {
    id :: non_neg_integer(),
    pid :: pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_n_processes(N) ->
    gen_server:call(?SERVER, {start_n_processes, N}, infinity).

get_n_siblings(N) when is_integer(N) ->
    Size = ets:info(?PROCESSES_TABLE, size),
    case Size of
        0 ->
            {ok, []};
        _ ->
            Ids = [rand:uniform(Size) - 1 || _ <- lists:seq(1, N)],
            Entries = [hd(ets:lookup(?PROCESSES_TABLE, Id)) || Id <- Ids],
%            io:format("Entries: ~p~n", [Entries]),
            {ok, [Pid || #proc_table_entry{pid=Pid} <- Entries]}
    end.
%get_n_siblings(N) when is_integer(N) ->
%    AllProcs = ets:tab2list(?PROCESSES_TABLE),
%    R = case AllProcs of
%        [] ->
%            [];
%        [{P}] ->
%            [P || _ <- lists:seq(1, N)];
%        _ ->
%            Rows = [lists:nth(crypto:rand_uniform(1, length(AllProcs) + 1), AllProcs) || _ <- lists:seq(1, N)],
%            [Pid || {Pid} <- Rows]
%    end,
%    {ok, R}.

get_settings() ->
    #{
        check_up_cycles => 10,
        n_destinations => 5
    }.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    ets:new(?PROCESSES_TABLE, [named_table, set, protected, {read_concurrency, true}, {keypos, #proc_table_entry.id}]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call({start_n_processes, N}, _From, #state{processes = Processes} = State) ->
    try
        NewPids = internal_start_n_processes(N, length(Processes)),
        {reply, ok, #state{processes = NewPids ++ Processes}}
    catch
        _:Reason ->
            {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
internal_start_n_processes(0, _) ->
    [];
internal_start_n_processes(N, StartingId) ->
    io:format("Starting ~p~n", [StartingId]),
    {ok, Pid} = supervisor:start_child(children_sup, [StartingId]),
    ets:insert(?PROCESSES_TABLE, #proc_table_entry{id=StartingId, pid=Pid}),
    [Pid | internal_start_n_processes(N - 1, StartingId + 1)].
