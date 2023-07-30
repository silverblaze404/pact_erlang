-module(pact_handler).
-behaviour(gen_server).

-export([start_pact/1, create_interaction/2, get_interaction/1, set_mock_server_port/2, get_mock_server_port/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(server_state, {
    pact_ref,
    interaction = #{},
    mock_server_port = undefined
}).

% Public API to start the server
start_pact(PactRef) ->
    gen_server:start_link({global, {PactRef, ?MODULE}}, ?MODULE, #server_state{pact_ref = PactRef}, []).

% Public API to create an interaction
create_interaction(PactRef, Interaction) ->
    gen_server:call({global, {PactRef, ?MODULE}}, {create_interaction, Interaction}).

% Public API to retrieve all interactions stored in the state
get_interaction(PactRef) ->
    gen_server:call({global, {PactRef, ?MODULE}}, get_interaction).

% Public API to set the mock server port when the server is started
set_mock_server_port(PactRef, Port) ->
    gen_server:call({global, {PactRef, ?MODULE}}, {set_mock_server_port, Port}).

% Public API to set the mock server port when the server is started
get_mock_server_port(PactRef) ->
    gen_server:call({global, {PactRef, ?MODULE}}, get_mock_server_port).

%% gen_server callbacks

init(#server_state{pact_ref = PactRef}) ->
    {ok, #server_state{pact_ref = PactRef}}.

handle_call({create_interaction, Interaction}, _From, State) ->
    NewState = State#server_state{interaction = Interaction},
    {reply, ok, NewState};

handle_call(get_interaction, _From, State) ->
    {reply, State#server_state.interaction, State};

handle_call({set_mock_server_port, Port}, _From, State) ->
    NewState = State#server_state{mock_server_port=Port},
    {reply, NewState, State};

handle_call(get_mock_server_port, _From, State) ->
    {reply, State#server_state.mock_server_port, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
