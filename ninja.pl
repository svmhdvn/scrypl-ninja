:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(dif)).

% This module implements a DCG parser/generator for a STRICT subset of a build.ninja file in the following way:
% * No comments
% * all words are separated by exactly one whitespace
% * there are no blank lines
% * in a rule declaration, required indented variables (e.g. 'command') come first

ninjafile([]) --> "".
ninjafile([D|Ds]) --> declaration(D), "\n", ninjafile(Ds).

% TODO support:
% * Order-only dependencies (||)
% * Validations (|@)
% * Implicit outputs (| before :)
declaration(rule(R, Command, Vs)) -->
  "rule ", word(R),
  "\n command = ", value_until_newline(Command),
  optional_indented_vars(Vs).
declaration(build_edge(Rule, Inputs, Outputs, Variables)) -->
  "build", build_outputs_list(Outputs),
  word(Rule), target_list(Inputs),
  optional_indented_vars(Variables).
declaration(variable(Name-Value)) --> word(Name), " = ", value_until_newline(Value).
declaration(default_targets(Targets)) --> "default", target_list(Targets).

optional_indented_vars([]) --> "".
optional_indented_vars([Name-Value|Vs]) -->
  "\n ", word(Name), " = ", value_until_newline(Value),
  optional_indented_vars(Vs).

target_list([]) --> "".
target_list([T|Ts]) --> " ", word(T), target_list(Ts).

build_outputs_list([]) --> ":".
build_outputs_list([O|Os]) --> " ", build_output(O), build_outputs_list(Os).

char_is_graphic(C) :- char_type(C, ascii_graphic).

not_colon(C) :- char_is_graphic(C), dif(':', C).

not_newline(C) :- dif('\n', C).

word(W) --> seq(W),
  { maplist(char_is_graphic, W) }.

value_until_newline(V) --> seq(V),
  { maplist(not_newline, V) }.

build_output(O) --> seq(O),
  { maplist(not_colon, O) }.
