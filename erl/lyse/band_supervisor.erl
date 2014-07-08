%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2014 10:59 PM
%%%-------------------------------------------------------------------
-module(band_supervisor).
-author("dfinlay").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Type :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Type) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
%% The band supervisor will allow its band members to make a few
%% mistakes before shutting down all operations, based on what
%% mood he's in. A lenient supervisor will tolerate more mistakes
%% than an angry supervisor, who'll tolerate more than a
%% complete jerk supervisor
init(lenient) ->
  init({one_for_one, 3, 60});
init(angry) ->
  init({rest_for_one, 2, 60});
init(jerk) ->
  init({one_for_all, 1, 60});
init({RestartStrategy, MaxRestart, MaxTime}) ->
  {ok, {{RestartStrategy, MaxRestart, MaxTime},
    [{singer,
      {musicians, start_link, [singer, good]},
      permanent, 1000, worker, [musicians]},
      {bass,
        {musicians, start_link, [bass, good]},
        temporary, 1000, worker, [musicians]},
      {drum,
        {musicians, start_link, [drum, bad]},
        transient, 1000, worker, [musicians]},
      {keytar,
        {musicians, start_link, [keytar, good]},
        transient, 1000, worker, [musicians]}
    ]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
