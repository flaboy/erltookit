-module(erltookit_app).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, start_supervisor/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start(_StartType, _StartArgs) ->
    ?MODULE:start_supervisor().

stop(_State) ->
    ok.

start_supervisor() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

