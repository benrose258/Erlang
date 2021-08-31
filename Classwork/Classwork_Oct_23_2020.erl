-module(ex).
-author("Ben Rose").
-compile(export_all).

fact(0) ->% Factorial when N==0, in erlang syntax
    1;
fact(N) when N>0 ->% Factorial when N > 0, same note about syntax as above. And always that note.
    N*fact(N-1).

%% Empty tree: {empty}
%% Non-Empty tree: {node,Data,LT,RT}

-type btree() :: {empty} |% Definition of a binary tree
 {node,number(),btree(),btree()}.

-spec t() -> btree(). % Definition of a new binary tree that is specified below
t() ->
    {node,12,
          {node,7,{empty},{empty}},
          {node,24,
  	           {node,18,{empty},{empty}},
    	         {empty}}}.

% Defining the sum of a binary tree that will return a number
-spec sum(btree()) -> number().
sum({empty}) ->% For an empty tree
    0;
sum({node,D,LT,RT}) ->% For a non-empty tree
    D + sum(LT) + sum(RT).

% Defining bump for a binary tree, which increases each number in the tree by one
bump({empty}) ->
    {empty};
bump({node,D,LT,RT}) ->
    {node, D+1,bump(LT),bump(RT)}.

% Defining mirror, which creates a mirrored tree to the given tree
mirror({empty}) ->
    {empty};
mirror({node,D,LT,RT}) ->
    {node,D,mirror(RT),mirror(LT)}.

% Defining bump, but for lists this time
bumpl([]) ->% Bump for empty list
    [];
bumpl([H | T]) ->% Bump for some head of the list H and the rest of the list (i.e. the tail) T
    [ H+1 | bumpl(T)].

%% Exercise: implement a function pre that given a binary tree produces the
%% pre-order traversal of the tree.
%% For example:
%%  > ex:pre(ex:t()).
%%  > [12,7,24,18].

% Definition of the function called "pre"
pre({empty}) ->% Pre for empty tree
  [];
pre({node,D,LT,RT}) ->% Pre for non-empty tree
  [ D | pre(LT)++pre(RT) ].
