
-module(mimer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    FileName = case os:getenv("MIMER_GLOB_FILE") of
                   false -> "/usr/share/mime/globs";
                   Any -> Any
               end,
    Processes = [{ mimer_server, {mimer_server, start_link, [FileName]},
           permanent, 5000, worker, dynamic}],
    {ok, { {one_for_one, 5, 10}, Processes} }.

