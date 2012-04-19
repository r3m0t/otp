%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_codeserver.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created :  4 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_codeserver).

-export([delete/0,
	 finalize_contracts/3,
         finalize_exported_types/2,
	 finalize_records/2,
	 get_contracts/0,
	 get_callbacks/1,
         get_exported_types/1,
	 get_exports/1,
	 get_records/0,
	 get_next_core_label/1,
	 get_temp_contracts/1,
         get_temp_exported_types/1,
	 get_temp_records/1,
	 insert/2,
	 insert_exports/2,
         insert_temp_exported_types/2,
	 is_exported/2,
	 lookup_mod_code/1,
	 lookup_mfa_code/1,
	 lookup_mod_records/1,
	 lookup_mod_contracts/1,
	 lookup_mfa_contract/1,
	 new/0,
	 set_next_core_label/2,
	 set_temp_records/2,
	 store_temp_records/3,
	 store_temp_contracts/4]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-export_type([codeserver/0]).

-include("dialyzer.hrl").

-define(CODESERVER, dialyzer_codeserver).
-define(CODESERVER_RECORDS, dialyzer_codeserver_records).
-define(CODESERVER_CONTRACTS, dialyzer_codeserver_contracts).

%%--------------------------------------------------------------------

-record(codeserver, {exported_types      = sets:new() :: set(), % set(mfa())
                     temp_exported_types = sets:new() :: set(), % set(mfa())
		     exports             = sets:new() :: set(), % set(mfa())
		     next_core_label     = 0          :: label(),
		     temp_records        = dict:new() :: dict(),
		     callbacks           = dict:new() :: dict(),
		     temp_contracts      = dict:new() :: dict(),
		     temp_callbacks      = dict:new() :: dict()
		    }).

-type codeserver() :: pid().

%%--------------------------------------------------------------------

call(Codeserver, Query) ->
  gen_server:call(Codeserver, Query).

cast(Codeserver, Msg) ->
  ok = gen_server:cast(Codeserver, Msg),
  Codeserver.

-spec init([]) -> {ok, #codeserver{}}.

init([]) ->
  CommonOptions = [public, named_table, {read_concurrency, true}],
  Names = [?CODESERVER_RECORDS, ?CODESERVER_CONTRACTS],
  Names = [ets:new(N, CommonOptions) || N <- Names],
  ?CODESERVER = ets:new(?CODESERVER, [compressed|CommonOptions]),
  {ok, #codeserver{}}.

-spec handle_call(Query::term(), From::term(), #codeserver{}) ->
        {reply, Reply::term(), #codeserver{}}.

handle_call(Query, _From, #codeserver{
		     callbacks = CallDict,
		     exported_types = ExpTypes,
		     exports = Exports,
		     next_core_label = NCL,
		     temp_callbacks = TempCallDict,
		     temp_contracts = TempContDict,
		     temp_exported_types = TempExpTypes,
		     temp_records = TempRecDict
		    } = Codeserver) ->
  Reply =
    case Query of
      {is_exported, MFA} ->
	sets:is_element(MFA, Exports);
      get_exported_types ->
	ExpTypes;
      get_temp_exported_types ->
	TempExpTypes;
      get_exports ->
	Exports;
      get_next_core_label ->
	NCL;
      get_temp_records ->
	TempRecDict;
      get_callbacks ->
	FunPreferNew = fun(_Key, _Val1, Val2) -> Val2 end,
	FunDictMerger =
	  fun(_Key, Value, AccIn) -> dict:merge(FunPreferNew, Value, AccIn) end,
	MergedCallbacks = dict:fold(FunDictMerger, dict:new(), CallDict),
	dict:to_list(MergedCallbacks);
      get_temp_contracts ->
	{TempContDict, TempCallDict}
    end,
  {reply, Reply, Codeserver}.

-spec handle_cast(Msg::term(), #codeserver{}) ->
        {noreply, #codeserver{}} | {stop, normal, #codeserver{}}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Msg, #codeserver{
	      exports = Exports,
	      temp_exported_types = TempExpTypes,
	      temp_callbacks = TempCallDict,
	      temp_contracts = TempContDict,
	      temp_records = TempRecDict
	     } = CS) ->
  NewCodeserver =
    case Msg of
      {insert_temp_exported_types, Set} ->
	NewExpTypes = sets:union(TempExpTypes, Set),
	CS#codeserver{temp_exported_types = NewExpTypes};
      {insert_exports, List} ->
	Set = sets:from_list(List),
	NewExports = sets:union(Exports, Set),
	CS#codeserver{exports = NewExports};
      {finalize_exported_types, Set} ->
	CS#codeserver{exported_types = Set, temp_exported_types = sets:new()};
      {set_next_core_label, NCL} ->
	CS#codeserver{next_core_label = NCL};
      {store_temp_records, Mod, Dict} ->
	CS#codeserver{temp_records = dict:store(Mod, Dict, TempRecDict)};
      {set_temp_records, Dict} ->
	CS#codeserver{temp_records = Dict};
      {store_temp_contracts, Mod, SpecDict} ->
	CS#codeserver{temp_contracts = dict:store(Mod, SpecDict, TempContDict)};
      {store_temp_callbacks, Mod, CallbackDict} ->
	CS#codeserver{temp_callbacks = dict:store(Mod, CallbackDict, TempCallDict)};
      {finalize_callbacks, CallbackDict} ->
	CS#codeserver{callbacks = CallbackDict,
		      temp_contracts = dict:new(),
		      temp_callbacks = dict:new()
		     }
    end,
  {noreply, NewCodeserver}.

-spec handle_info(term(), #codeserver{}) -> {noreply, #codeserver{}}.

handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), #codeserver{}) -> ok.

terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), #codeserver{}, term()) -> {ok, #codeserver{}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%-------------------------------------------------------------------------------

-spec new() -> codeserver().

new() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  Pid.

-spec delete() -> 'true'.

delete() ->
  ets:delete(?CODESERVER),
  ets:delete(?CODESERVER_CONTRACTS),
  ets:delete(?CODESERVER_RECORDS).

-spec insert(atom(), cerl:c_module()) -> 'true'.

insert(Mod, ModCode) ->
  Name = cerl:module_name(ModCode),
  Exports = cerl:module_exports(ModCode),
  Attrs = cerl:module_attrs(ModCode),
  Defs = cerl:module_defs(ModCode),
  As = cerl:get_ann(ModCode),
  Funs =
    [{{Mod, cerl:fname_id(Var), cerl:fname_arity(Var)},
      Val} || Val = {Var, _Fun} <- Defs],
  Keys = [Key || {Key, _Value} <- Funs],
  ets:insert(?CODESERVER, [{Mod, {Name, Exports, Attrs, Keys, As}} | Funs]).

-spec insert_temp_exported_types(set(), codeserver()) -> codeserver().

insert_temp_exported_types(Set, CS) ->
  cast(CS, {insert_temp_exported_types, Set}).

-spec insert_exports([mfa()], codeserver()) -> codeserver().

insert_exports(List, CS) ->
  cast(CS, {insert_exports, List}).

-spec is_exported(mfa(), codeserver()) -> boolean().

is_exported(MFA, CS) ->
  call(CS, {is_exported, MFA}).

-spec get_exported_types(codeserver()) -> set(). % set(mfa())

get_exported_types(CS) ->
  call(CS, get_exported_types).

-spec get_temp_exported_types(codeserver()) -> set().

get_temp_exported_types(CS) ->
  call(CS, get_temp_exported_types).

-spec get_exports(codeserver()) -> set().  % set(mfa())

get_exports(CS) ->
  call(CS, get_exports).

-spec finalize_exported_types(set(), codeserver()) -> codeserver().

finalize_exported_types(Set, CS) ->
  cast(CS, {finalize_exported_types, Set}).

-spec lookup_mod_code(atom()) -> cerl:c_module().

lookup_mod_code(Mod) when is_atom(Mod) ->
  code_table_lookup(Mod).

-spec lookup_mfa_code(mfa()) -> {cerl:c_var(), cerl:c_fun()}.

lookup_mfa_code({_M, _F, _A} = MFA) ->
  code_table_lookup(MFA).

-spec get_next_core_label(codeserver()) -> label().

get_next_core_label(CS) ->
  call(CS, get_next_core_label).

-spec set_next_core_label(label(), codeserver()) -> codeserver().

set_next_core_label(NCL, CS) ->
  cast(CS, {set_next_core_label, NCL}).

-spec lookup_mod_records(atom()) -> dict().

lookup_mod_records(Mod) when is_atom(Mod) ->
  case ets_lookup_dict(Mod, ?CODESERVER_RECORDS) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec get_records() -> dict().

get_records() ->
  dict:from_list(ets:tab2list(?CODESERVER_RECORDS)).

-spec store_temp_records(atom(), dict(), codeserver()) -> codeserver().

store_temp_records(Mod, Dict, CS) when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> cast(CS, {store_temp_records, Mod, Dict})
  end.

-spec get_temp_records(codeserver()) -> dict().

get_temp_records(CS) ->
  call(CS, get_temp_records).

-spec set_temp_records(dict(), codeserver()) -> codeserver().

set_temp_records(Dict, CS) ->
  cast(CS, {set_temp_records, Dict}).

-spec finalize_records(dict(), codeserver()) -> codeserver().

finalize_records(Dict, CS) ->
  ets:insert(?CODESERVER_RECORDS, dict:to_list(Dict)),
  CS.

-spec lookup_mod_contracts(atom()) -> dict().

lookup_mod_contracts(Mod) when is_atom(Mod) ->
  case ets_lookup_dict(Mod, ?CODESERVER_CONTRACTS) of
    error -> dict:new();
    {ok, Keys} ->
      dict:from_list([get_contract_pair(Key)|| Key <- Keys])
  end.

get_contract_pair(Key) ->
  {Key, ets:lookup_element(?CODESERVER_CONTRACTS, Key, 2)}.

-spec lookup_mfa_contract(mfa()) ->
         'error' | {'ok', dialyzer_contracts:file_contract()}.

lookup_mfa_contract(MFA) ->
  ets_lookup_dict(MFA, ?CODESERVER_CONTRACTS).

-spec get_contracts() -> dict().

get_contracts() ->
  dict:from_list(ets:tab2list(?CODESERVER_CONTRACTS)).

-spec get_callbacks(codeserver()) -> list().

get_callbacks(CS) ->
  call(CS, get_callbacks).

-spec store_temp_contracts(atom(), dict(), dict(), codeserver()) ->
	 codeserver().

store_temp_contracts(Mod, SpecDict, CallbackDict, CS) when is_atom(Mod) ->
  CS1 =
    case dict:size(SpecDict) =:= 0 of
      true -> CS;
      false -> cast(CS, {store_temp_contracts, Mod, SpecDict})
    end,
  case dict:size(CallbackDict) =:= 0 of
    true -> CS1;
    false -> cast(CS1, {store_temp_callbacks, Mod, CallbackDict})
  end.

-spec get_temp_contracts(codeserver()) -> {dict(), dict()}.

get_temp_contracts(CS) ->
  call(CS, get_temp_contracts).

-spec finalize_contracts(dict(), dict(), codeserver()) -> codeserver().

finalize_contracts(SpecDict, CallbackDict, CS)  ->
  true = dict:fold(fun decompose_spec_dict/3, true, SpecDict),
  cast(CS, {finalize_callbacks, CallbackDict}).

decompose_spec_dict(Mod, Dict, true) ->
  Keys = dict:fetch_keys(Dict),
  ets:insert(?CODESERVER_CONTRACTS, dict:to_list(Dict)),
  ets:insert(?CODESERVER_CONTRACTS, {Mod, Keys}).

code_table_lookup(M) when is_atom(M) ->
  {Name, Exports, Attrs, Keys, As} = ets_lookup(M),
  Defs = [code_table_lookup(Key) || Key <- Keys],
  cerl:ann_c_module(As, Name, Exports, Attrs, Defs);
code_table_lookup(MFA) ->
  ets_lookup(MFA).

ets_lookup(Key) ->
  ets:lookup_element(?CODESERVER, Key, 2).

ets_lookup_dict(Key, Table) ->
  try ets:lookup_element(Table, Key, 2) of
      Val -> {ok, Val}
  catch
    _:_ -> error
  end.
