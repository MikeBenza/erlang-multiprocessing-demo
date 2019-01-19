%%%-------------------------------------------------------------------
%% @doc processes top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(processes_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    % Supervisor structure:
    % processes_sup (one for one)
    % - record_srv
    % - report_srv
    % - children_sup (simple one for okne)
    %   - child
    Children = [
        {process_manager, {process_manager, start_link, []}, permanent, 6000, worker, [process_manager]},
        {announcer, {announcer, start_link, []}, permanent, 6000, worker, [announcer]},
        {children_sup, {children_sup, start_link, []}, permanent, 6000, supervisor, [children_sup]}
    ],
    {ok, {{one_for_all, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
