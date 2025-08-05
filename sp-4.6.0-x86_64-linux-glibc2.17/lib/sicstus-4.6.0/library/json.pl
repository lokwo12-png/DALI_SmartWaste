/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
% Copyright (C) 2020, RISE Research Institutes of Sweden AB.


% FIXME: Verify that write_canonical/2 always emits JSON-compatible syntax in write_number/3.
% want: Better error handling in option processing
% want: Better error handling for malformed input when writing
% TODO: What to do with lone surrogate, or control character, when reading string?
% TODO: What to do with lone escaped surrogate, e.g. "xy\u0xDC00z", when reading string?
% TODO: More detailed errors (e.g. stream position when syntax error)


% Feature creep ideas:
% Sort keys
% Pretty printing
% Skip unhandled keys
% A way to serialize Prolog terms to strings, e.g. #(Term)


%@  This library module provides some utilities for reading and writing structured data using the
%@  @uref{https://json.org/, JSON} (JavaScript Object Notation) serialization format.
%@  The library module is part of SICStus Prolog since release 4.5.0.
%@
%@  JSON is a light-weight, language independent, data-interchange format with good support in many environments.
%@  As such, it is a convenient format when transferring data between Prolog and other programming languages.
%@  The format is specified in @uref{https://www.ecma-international.org/publications/standards/Ecma-404.htm, , ECMA-404}
%@  and in @uref{https://www.rfc-editor.org/info/rfc8259, , RFC 8259}.
%@
%@  The Prolog representation of JSON values is as follows:
%@  @table @asis
%@
%@  @item @var{Number}
%@  A JSON number is represented as the corresponding Prolog number; as a floating point number
%@  when the JSON number has an exponent or a fractional part, otherwise as an integer.
%@
%@  @item @var{String}
%@  A JSON string is represented as the corresponding Prolog atom (escaped surrogate pairs are combined into the corresponding Unicode code point).
%@
%@  @item @var{Array}
%@  A JSON array is represented as a list of the corresponding Prolog terms.
%@
%@  @item @var{Object}
%@  A JSON object is a sequence of @code{@var{name}:@var{value}} pairs, where each name is a JSON string and each value is an arbitrary JSON value.
%@  It is represented as a term @code{json(@var{Members})} where @var{Members} is a list of @code{@var{Name}=@var{Value}} pairs,
%@  where @var{Name} is a representation of the JSON string name and @var{Value} is a representaton of the JSON value.
%@
%@  @item @code{null}
%@  @itemx @code{true}
%@  @itemx @code{false}
%@  These special JSON literals are, by default, translated to the Prolog terms @code{@@(null)},
%@  @code{@@(true)}, and @code{@@(false)}, respectively.
%@  @end table
%@
%@  Examples:
%@  @example
%@  @group
%@  @kbd{123} @result{} @code{123}
%@
%@  @kbd{12.3} @result{} @code{12.3}
%@
%@  @kbd{12E3} @result{} @code{12.0E3}
%@
%@  @kbd{"foo"} @result{} @code{'foo'}
%@
%@  @kbd{null} @result{} @code{@@(null)}
%@
%@  @kbd{["a", 2, "bar"]} @result{} @code{['a', 2, 'bar']}
%@
%@  @kbd{@{"age": 42,}
%@  @kbd{ "name": @{"first":"Kim", "last":"Jones"@},}
%@  @kbd{ "children": ["Lisa","Jim","John"]}
%@  @kbd{@}}
%@  @result{}
%@  @code{json(['age'=42,
%@        'name'=json(['first'='Kim', 'last'='Jones']),
%@        'children'=['Lisa','Jim','John']])}
%@
%@  @end group
%@  @end example
%@
%@
%@  It is possible to specify other Prolog representations of a JSON value using the option argument. See below for details.
%@
%@ @subsection Options
%@
%@  The following options are used. They are valid for all predicates that takes options, unless stated otherwise.
%@
%@  @table @code
%@  @item compact(@var{Boolean})
%@  Valid values for @var{Boolean} are @code{true} and @code{false} (default).
%@
%@  If @code{false} (default), JSON values are written with extra whitespace and end-of-line characters to make it easier for humans to read.
%@  The details of the non-compact format is subject to change without notice.
%@
%@  If @code{true}, JSON values are written with a minimum of whitespace.
%@  Since this implies that no end-of-line characters will be written, it makes it possible to read the resulting JSON as a single line.
%@
%@  Only valid for predicates that write.
%@
%@  @item ascii(@var{Boolean})
%@  Valid values for @var{Boolean} are @code{true} (default) and @code{false}.
%@
%@  If @code{true} (default), JSON values are written using only 7-bit ASCII characters, which makes the format less sensitive to stream encodings.
%@
%@  If @code{false}, JSON values are written using full Unicode. In this case any streams should use UTF-8 encoding.
%@
%@  Only valid for predicates that write.
%@
%@  @item null(@var{X})
%@  @itemx true(@var{X})
%@  @itemx false(@var{X})
%@  The specified term @var{X}, which may be a variable, is used for representing the corresponding JSON literal.
%@
%@  @item array_tag(@var{Tag})
%@  The @var{Tag} must be an atom.
%@
%@  A JSON array is represented as the compound term @code{@var{Tag}(@var{Elements})}, where @var{Elements} is a list of the representations of the array elements.
%@  This may be useful if you need to be able to distinguish between an empty JSON array (@code{[]}), and a JSON string @code{"[]"},
%@  since these have the same Prolog representation (the atom @code{[]}) in the default representation.
%@
%@  If this option is not specified (the default), then JSON arrays are represented as a list of the representations of the array elements.
%@
%@  @item object_tag(@var{Tag})
%@  The @var{Tag} must be an atom. @var{Tag} defaults to @code{'json'}.
%@
%@  A JSON object is represented as the compound term @code{@var{Tag}(@var{Members})},
%@  where @var{Members} is a list of @code{@var{Name}=@var{Value}} pairs,
%@  where @var{Name} is a representation of the JSON string name and @var{Value} is a representaton of the JSON value.
%@
%@  @item width(@var{Width})
%@  This option is present for compatibility with other systems.
%@
%@  If @var{Width} is @code{0} (zero), it is treated as a synonym for @code{compact(true)}. Otherwise, the option is currently ignored.
%@
%@  Only valid for predicates that write.
%@
%@  @item value_string_as(@var{Value})
%@  @itemx step(@var{Value})
%@  @itemx tab(@var{Value})
%@  @itemx serialize_unknown(@var{Value})
%@
%@  These options are present for compatibility with other systems. They are currently ignored.
%@
%@  Only valid for predicates that write.
%@  @end table
%@
:- module(json, [json_read/3,json_read/2,
                 json_write/3,json_write/2,
                 is_json_term/1,is_json_term/2]).

%@  @subsection Examples
%@  The folder @file{library/json/examples/} contains examples and utilities for using JSON.
%@
%@  @subsubsection Process Communication
%@
%@  In @file{library/json/examples/} there are several examples of using JSON to communicate between a non-Prolog parent process and a SICStus sub-process.
%@  There are examples of writing the parent process using Python, C#, Java, C, Prolog etc.@:.
%@  These examples provide functionality that is similar to what is available in the language-specific
%@  libraries, e.g. @code{library(jasper)} and @code{library(prologbeans)}, but do so in a language-agnostic way.
%@
%@  @subsubsection JSON Text as Atoms and Character Lists
%@
%@  All functionality in @code{library(json)} read and write from text streams.
%@  Sometimes it is convenient to read from a list of character codes etc.@:, which can easily be implemented using @code{library(codesio)}.
%@
%@
%@  Example code for this kind of functionality is available in @code{library('json/examples/json_codes')}.
%@
%@  @example
%@  @group
%@  | ?- use_module(library('json/examples/json_codes')).
%@  @dots{}
%@  % compiled @dots{}/json_codes.pl in module json_codes, @dots{}
%@  yes
%@  | ?- JSONCodes = "@{\"foo\": 42, \"bar\": null@}",
%@       json_from_codes(JSONCodes, JSONTerm),
%@       json_to_atom(JSONTerm, JSONAtom, [compact(true)]).
%@  JSONCodes = [123,34,102,111,111,34,58,32,52,50|...],
%@  JSONTerm = json([foo=42,bar= @@(null)]),
%@  JSONAtom = '@{"foo":42,"bar":null@}' ?
%@  yes
%@  | ?-
%@  @end group
%@  @end example
%@


%@
%@  @subsection Exported Predicates
%@
%@  The @var{Options} argument is described in the module documentation.
%@  All @var{Options}, both for read and write predicates, are allowed. Irrelevant options are simply ignored.
%@
%@  @table @code
%@
:- use_module(library(lists), [is_list/1]).

:- json_read(+Stream, -Term, +Options) is det.
:- json_read(+Stream, -Term) is det.

:- json_write(+Stream, +Term, +Options) is det.
:- json_write(+Stream, +Term) is det.

:- is_json_term(+Term) is semidet.
:- is_json_term(+Term, +Options) is semidet.

%@  @item json_read(@var{+Stream}, @var{-Term})
%@  @itemx json_read(@var{+Stream}, @var{-Term}, @var{+Options})
%@  @PLXindex {json_read/[2,3] (json)}
%@  Reads a single JSON value from the stream @var{Stream}
%@  and unifies it with @var{Term}.
json_read(Stream, Term, Options) :-
        (   parse_options(Options, Opts, read) ->
            true
        ;   otherwise ->
            throw_option_error(Options, json_read(Stream,Term,Options))
        ),
        (   read_value(Term, Stream, Opts) ->
            true
        ;   otherwise ->
            throw_json_type_error(Term, json_read(Stream,Term,Options))
        ).


:- json_read/2 is documented_as(json_read/3).
json_read(Stream, Term) :-
        default_options(Opts, read),
        (   read_value(Term, Stream, Opts) ->
            true
        ;   otherwise ->
            throw_json_type_error(Term, json_read(Stream,Term))
        ).


%@  @item json_write(@var{+Stream}, @var{+Term})
%@  @itemx json_write(@var{+Stream}, @var{+Term}, @var{+Options})
%@  @PLXindex {json_write/[2,3] (json)}
%@  Write the JSON value @var{Term} to the stream @var{Stream}.
json_write(Stream, Term, Options) :-
        (   parse_options(Options, Opts, write) ->
            true
        ;   otherwise ->
            throw_option_error(Options, json_write(Stream,Term,Options))
        ),
        (   write_value(Term, Stream, Opts) ->
            true
        ;   otherwise ->
            throw_json_type_error(Term, json_write(Stream,Term,Options))
        ).


:- json_write/2 is documented_as(json_write/3).
json_write(Stream, Term) :-
        default_options(Opts, write),
        (   write_value(Term, Stream, Opts) ->
            true
        ;   otherwise ->
            throw_json_type_error(Term, json_write(Stream,Term))
        ).


%@  @item is_json_term(@var{+Term})
%@  @itemx is_json_term(@var{+Term}, @var{+Options})
%@  @PLXindex {is_json_term/[1,2] (json)}
%@  True if the @var{Term} is a valid representation of a JSON value.
is_json_term(Term, Options) :-
        (   parse_options(Options, Opts, write) ->
            true
        ;   otherwise ->
            throw_option_error(Options, is_json_term(Term,Options))
        ),
        is_value(Term, Opts).


:- is_json_term/1 is documented_as(is_json_term/2).
is_json_term(Term) :-
        is_json_term(Term, []).


parse_options(Options, Opts, Mode) :-
        default_options(Opts1, Mode),
        parse_options1(Options, Opts1, Opts, Mode).


parse_options1(Options, _Opts1, _Opts, _Mode) :-
        var(Options),
        !,
        fail.
parse_options1([], Opts1, Opts, _Mode) :-
        Opts = Opts1.
parse_options1([Option|Options], Opts1, Opts, Mode) :-
        parse_option(Option, Opts1, Opts2, Mode),
        parse_options1(Options, Opts2, Opts, Mode).


parse_option(Option, _Opts, _Opts1, _Mode) :-
        var(Option),
        !,
        fail.
parse_option(null(NullTerm), Opts1, Opts, _Mode) :-
        % We do _not_ require a ground term, currently (And a unique variable makes some sense).
        with_option(null, NullTerm, Opts1, Opts).
parse_option(true(TrueTerm), Opts1, Opts, _Mode) :-
        % We do _not_ require a ground term, currently (And a unique variable makes some sense).
        with_option(true, TrueTerm, Opts1, Opts).
parse_option(false(FalseTerm), Opts1, Opts, _Mode) :-
        % We do _not_ require a ground term, currently (And a unique variable makes some sense).
        with_option(false, FalseTerm, Opts1, Opts).
parse_option(compact(Boolean), Opts1, Opts, write) :-
        with_option(compact, Boolean, Opts1, Opts).
parse_option(ascii(Boolean), Opts1, Opts, write) :-
        with_option(ascii, Boolean, Opts1, Opts).
parse_option(object_tag(Tag), Opts1, Opts, _Mode) :-
        atom(Tag),
        with_option(object_tag, Tag, Opts1, Opts).
parse_option(array_tag(Tag), Opts1, Opts, _Mode) :-
        atom(Tag),
        with_option(array_tag, Tag, Opts1, Opts).
% compatibility
parse_option(width(Width), Opts1, Opts, write) :-
        % Treat width(N) as compact iff N == 0
        (   Width == 0 ->
            Boolean = true
        ;   Boolean = false
        ),
        with_option(compact, Boolean, Opts1, Opts).
% compatibility
parse_option(value_string_as(_Value), Opts1, Opts, write) :-
        % Ignore
        Opts = Opts1.
% compatibility
parse_option(step(_Value), Opts1, Opts, write) :-
        % Ignore
        Opts = Opts1.
% compatibility
parse_option(tab(_Value), Opts1, Opts, write) :-
        % Ignore
        Opts = Opts1.
% compatibility
parse_option(serialize_unknown(_Value), Opts1, Opts, write) :-
        % FIXME: Implement (writeq-to-string unless Term is variable). (But I prefer something operator-independent, like write_canonical).
        % Ignore
        Opts = Opts1.


with_option(Key, _Value, _Opts1, _Opts) :-
        var(Key),
        !,
        fail.
with_option(null, Value, opt7(ATag,OTag,_Null,True,False,Compact,ASCII), opt7(ATag,OTag,Value,True,False,Compact,ASCII)).
with_option(true, Value, opt7(ATag,OTag,Null,_True,False,Compact,ASCII), opt7(ATag,OTag,Null,Value,False,Compact,ASCII)).
with_option(false, Value, opt7(ATag,OTag,Null,True,_False,Compact,ASCII), opt7(ATag,OTag,Null,True,Value,Compact,ASCII)).
with_option(compact, Value, opt7(ATag,OTag,Null,True,False,_Compact,ASCII), opt7(ATag,OTag,Null,True,False,Value,ASCII)).
with_option(ascii, Value, opt7(ATag,OTag,Null,True,False,Compact,_ASCII), opt7(ATag,OTag,Null,True,False,Compact,Value)).
with_option(array_tag, Value, opt7(_ATag,OTag,Null,True,False,Compact,ASCII), opt7(Value,OTag,Null,True,False,Compact,ASCII)).
with_option(object_tag, Value1, opt7(ATag,_OTag,Null,True,False,Compact,ASCII), opt7(ATag,Value1,Null,True,False,Compact,ASCII)).


default_options(opt7(ATag,OTag,Null,True,False,Compact,ASCII), _Mode) :-
        default_option(array_tag, ATag),
        default_option(object_tag, OTag),
        default_option(null, Null),
        default_option(true, True),
        default_option(false, False),
        default_option(compact, Compact),
        default_option(ascii, ASCII).

default_option(array_tag, _).   % Variable means no tag
default_option(object_tag, json). % Variable means no tag
default_option(null, @(null)).
default_option(true, @(true)).
default_option(false, @(false)).
default_option(compact, false).
default_option(ascii, true).

% Get the option value associated with key
:- opt_value(+Key, +Opts, -Value) is det.
opt_value(Key, _Opts, _Value) :-
        var(Key),
        !,
        fail.
opt_value(null, opt7(_ATag,_OTag,Null,_True,_False,_Compact,_ASCII), Null).
opt_value(true, opt7(_ATag,_OTag,_Null,True,_False,_Compact,_ASCII), True).
opt_value(false, opt7(_ATag,_OTagg,_Null,_True,False,_Compact,_ASCII), False).
opt_value(compact, opt7(_ATag,_OTag,_Null,_True,_False,Compact,_ASCII), Compact).
opt_value(ascii, opt7(_ATag,_OTag,_Null,_True,_False,_Compact,ASCII), ASCII).
opt_value(array_tag, opt7(ATag,_OTag,_Null,_True,_False,_Compact,_ASCII), ATag).
opt_value(object_tag, opt7(_ATag,OTag,_Null,_True,_False,_Compact,_ASCII), OTag).


read_value(Term, Stream, Opts) :-
        get_code(Stream, C),
        read_value_dispatch(C, Term, Stream, Opts).

read_value_dispatch(-1, _Term, Stream, Opts) :-
        throw_unexpected_end_of_file(Stream, Opts).
read_value_dispatch(0'{, Term, Stream, Opts) :-
        read_object_continue(Term, Stream, Opts).
read_value_dispatch(0'", Term, Stream, Opts) :-
        read_string_continue(Term, Stream, Opts).
read_value_dispatch(0'[, Term, Stream, Opts) :-
        read_array_continue(Term, Stream, Opts).
read_value_dispatch(0'n, Term, Stream, Opts) :-
        expect_remaining_codes("null", Stream, Opts),
        opt_value(null, Opts, Term).
read_value_dispatch(0't, Term, Stream, Opts) :-
        expect_remaining_codes("true", Stream, Opts),
        opt_value(true, Opts, Term).
read_value_dispatch(0'f, Term, Stream, Opts) :-
        expect_remaining_codes("false", Stream, Opts),
        opt_value(false, Opts, Term).
read_value_dispatch(0'-, Term, Stream, Opts) :-
        peek_code(Stream, PC),
        (   0'0 =< PC,
            PC =< 0'9 ->
            get_code(Stream, _PC),
            read_unsigned_number(PC, Unsigned, Stream, Opts),
            Term is (-Unsigned)
        ;   PC == -1 ->
            get_code(Stream, _), % Get the EOF
            throw_unexpected_end_of_file(Stream, Opts)
        ;   otherwise ->         % minus followed by something else, leave it in the stream
            throw_number_syntax_error(PC, Stream, Opts)
        ).
read_value_dispatch(0'0, Term, Stream, Opts) :-
        read_unsigned_number(0'0, Term, Stream, Opts).
read_value_dispatch(0'1, Term, Stream, Opts) :-
        read_unsigned_number(0'1, Term, Stream, Opts).
read_value_dispatch(0'2, Term, Stream, Opts) :-
        read_unsigned_number(0'2, Term, Stream, Opts).
read_value_dispatch(0'3, Term, Stream, Opts) :-
        read_unsigned_number(0'3, Term, Stream, Opts).
read_value_dispatch(0'4, Term, Stream, Opts) :-
        read_unsigned_number(0'4, Term, Stream, Opts).
read_value_dispatch(0'5, Term, Stream, Opts) :-
        read_unsigned_number(0'5, Term, Stream, Opts).
read_value_dispatch(0'6, Term, Stream, Opts) :-
        read_unsigned_number(0'6, Term, Stream, Opts).
read_value_dispatch(0'7, Term, Stream, Opts) :-
        read_unsigned_number(0'7, Term, Stream, Opts).
read_value_dispatch(0'8, Term, Stream, Opts) :-
        read_unsigned_number(0'8, Term, Stream, Opts).
read_value_dispatch(0'9, Term, Stream, Opts) :-
        read_unsigned_number(0'9, Term, Stream, Opts).

read_value_dispatch(0x0009, Term, Stream, Opts) :-
        read_value(Term, Stream, Opts).
read_value_dispatch(0x000A, Term, Stream, Opts) :-
        read_value(Term, Stream, Opts).
read_value_dispatch(0x000D, Term, Stream, Opts) :-
        read_value(Term, Stream, Opts).
read_value_dispatch(0x0020, Term, Stream, Opts) :-
        read_value(Term, Stream, Opts).

read_array_continue(Term, Stream, Opts) :-
        opt_value(array_tag, Opts, ATag),
        (   var(ATag) ->
            Term = Elements     % no tag
        ;   functor(Term, ATag, 1),
            arg(1, Term, Elements)
        ),
        read_array_elements(Elements, Stream, Opts).

% array = <begin-array> [ value *( value-separator value ) ] <end-array>
% Has read ( <begin-array> *( value value-separator ) )
% Looking for <value> or <end-array>
read_array_elements(Elements, Stream, Opts) :-
        get_non_whitespace_code(Stream, C),
        (   C == 0'] ->
            Elements = []
        ;   otherwise ->
            read_value_dispatch(C, Element, Stream, Opts),
            Elements = [Element|Elements1],
            read_array_prefixed_elements(Elements1, Stream, Opts)
        ).

% array = <begin-array> [ value *( value-separator value ) ] <end-array>
% Has read ( <begin-array> value *( value-separator value ) )
% Looking for <value-separator> or <end-array>
read_array_prefixed_elements(Elements, Stream, Opts) :-
        get_non_whitespace_code(Stream, C),
        read_prefixed_array_elements_dispatch(C, Elements, Stream, Opts).

read_prefixed_array_elements_dispatch(EOF, _Elements, Stream, Opts) :-
        EOF = -1,
        !,
        throw_unexpected_end_of_file(Stream, Opts).
read_prefixed_array_elements_dispatch(VALUE_SEPARATOR, Elements, Stream, Opts) :-
        VALUE_SEPARATOR = (0',),
        !,
        read_array_elements(Elements, Stream, Opts).
read_prefixed_array_elements_dispatch(END_ARRAY, Elements, _Stream, _Opts) :-
        END_ARRAY = 0'],
        !,
        Elements = [].
read_prefixed_array_elements_dispatch(_C, _Elements, Stream, Opts) :-
        throw_syntax_error(Stream, Opts).


% Have read <{>
read_object_continue(Term, Stream, Opts) :-
        opt_value(object_tag, Opts, OTag),
        (   var(OTag) ->
            Term = Members      % no tag
        ;   functor(Term, OTag, 1),
            arg(1, Term, Members)
        ),
        read_object_members(Members, Stream, Opts).


read_object_members(Members, Stream, Opts) :-
        get_code(Stream, C),
        read_object_members_dispatch(C, Members, Stream, Opts).

read_object_members_dispatch(-1, _Members, Stream, Opts) :-
        throw_unexpected_end_of_file(Stream, Opts).
read_object_members_dispatch(0'}, Members, _Stream, _Opts) :-
        !,
        Members = [].
read_object_members_dispatch(C, Members, Stream, Opts) :-
        C == 0x0009,            % character tabulation (U+0009).
        !,
        read_object_members(Members, Stream, Opts).
read_object_members_dispatch(C, Members, Stream, Opts) :-
        C == 0x000A,            % line feed (U+000A).
        !,
        read_object_members(Members, Stream, Opts).
read_object_members_dispatch(C, Members, Stream, Opts) :-
        C == 0x000D,            % carriage return (U+000D)
        !,
        read_object_members(Members, Stream, Opts).
read_object_members_dispatch(C, Members, Stream, Opts) :-
        C == 0x0020,            % space (U+0020).
        !,
        read_object_members(Members, Stream, Opts).
read_object_members_dispatch(0'", Members, Stream, Opts) :-
        !,
        Members = [Member|Members1],
        read_object_member_continue(Member, Stream, Opts),
        get_code(Stream, Lookahead),
        read_object_members_tail_dispatch(Lookahead, Members1, Stream, Opts).


% Has read at least one member (so expect <,> or <}>)
read_object_members_tail(Members, Stream, Opts) :-
        get_code(Stream, C),
        read_object_members_tail_dispatch(C, Members, Stream, Opts).


read_object_members_tail_dispatch(-1, _Members, Stream, Opts) :-
        throw_unexpected_end_of_file(Stream, Opts).
read_object_members_tail_dispatch(0'}, Members, _Stream, _Opts) :-
        !,
        Members = [].
read_object_members_tail_dispatch(0',, Members, Stream, Opts) :-
        !,
        Members = [Member|Members1],
        read_object_member(Member, Stream, Opts),
        read_object_members_tail(Members1, Stream, Opts).
read_object_members_tail_dispatch(C, Members, Stream, Opts) :-
        C == 0x0009,            % character tabulation (U+0009).
        !,
        read_object_members_tail(Members, Stream, Opts).
read_object_members_tail_dispatch(C, Members, Stream, Opts) :-
        C == 0x000A,            % line feed (U+000A).
        !,
        read_object_members_tail(Members, Stream, Opts).
read_object_members_tail_dispatch(C, Members, Stream, Opts) :-
        C == 0x000D,            % carriage return (U+000D)
        !,
        read_object_members_tail(Members, Stream, Opts).
read_object_members_tail_dispatch(C, Members, Stream, Opts) :-
        C == 0x0020,            % space (U+0020).
        !,
        read_object_members_tail(Members, Stream, Opts).


read_object_member(Member, Stream, Opts) :-
        get_code(Stream, C),
        read_object_member_dispatch(C, Member, Stream, Opts).

read_object_member_dispatch(-1, _Member, Stream, Opts) :-
        !,
        throw_unexpected_end_of_file(Stream, Opts).
read_object_member_dispatch(0'", Member, Stream, Opts) :-
        !,
        read_object_member_continue(Member, Stream, Opts).
read_object_member_dispatch(C, Member, Stream, Opts) :-
        C == 0x0009,            % character tabulation (U+0009).
        !,
        read_object_member(Member, Stream, Opts).
read_object_member_dispatch(C, Member, Stream, Opts) :-
        C == 0x000A,            % line feed (U+000A).
        !,
        read_object_member(Member, Stream, Opts).
read_object_member_dispatch(C, Member, Stream, Opts) :-
        C == 0x000D,            % carriage return (U+000D)
        !,
        read_object_member(Member, Stream, Opts).
read_object_member_dispatch(C, Member, Stream, Opts) :-
        C == 0x0020,            % space (U+0020).
        !,
        read_object_member(Member, Stream, Opts).
read_object_member_dispatch(_C, _Member, Stream, Opts) :-
        throw_syntax_error(Stream, Opts).


% Has read " (start of string name)
read_object_member_continue(Member, Stream, Opts) :-
        Member = (Name=Value),
        read_string_continue(Name, Stream, Opts),
        read_member_colon(Stream, Opts),
        read_value(Value, Stream, Opts).


read_member_colon(Stream, Opts) :-
        get_code(Stream, C),
        read_member_colon_dispatch(C, Stream, Opts).

read_member_colon_dispatch(-1, Stream, Opts) :-
        throw_unexpected_end_of_file(Stream, Opts).
read_member_colon_dispatch(0':, _Stream, _Opts) :-
        !.
read_member_colon_dispatch(C, Stream, Opts) :-
        C == 0x0009,            % character tabulation (U+0009).
        !,
        read_member_colon(Stream, Opts).
read_member_colon_dispatch(C, Stream, Opts) :-
        C == 0x000A,            % line feed (U+000A).
        !,
        read_member_colon(Stream, Opts).
read_member_colon_dispatch(C, Stream, Opts) :-
        C == 0x000D,            % carriage return (U+000D)
        !,
        read_member_colon(Stream, Opts).
read_member_colon_dispatch(C, Stream, Opts) :-
        C == 0x0020,            % space (U+0020).
        !,
        read_member_colon(Stream, Opts).
read_member_colon_dispatch(_C, Stream, Opts) :-
        throw_syntax_error(Stream, Opts).


% "      number = [ minus ] int [ frac ] [ exp ]"
% Note that fractional part is optional also when exponent is present
% NOTE: Floating point reading is tricky, so we must use number_codes/2 for that so we do not introduce inaccuracies.
read_unsigned_number(Digit, Number, Stream, Opts) :-
        get_decimal_digit_codes(IntegralDigits1, Stream, Opts),
        IntegralDigits = [Digit|IntegralDigits1],
        read_number_optional_fraction(FractionDigits, Stream, Opts),
        read_number_optional_exponent(Exponent, Stream, Opts),
        (   FractionDigits = [],
            Exponent = [] ->
            NumberCodes = IntegralDigits
        ;   otherwise ->
            (   FractionDigits = [] ->
                Fraction = ".0"
            ;   Fraction = [0'.|FractionDigits]
            ),
            append(Fraction, Exponent, FractionExponent),
            append(IntegralDigits, FractionExponent, NumberCodes)
        ),
        number_codes(Number, NumberCodes).

% "frac = decimal-point 1*DIGIT"
read_number_optional_fraction(FractionDigits, Stream, Opts) :-
        peek_code(Stream, PC),
        (   PC == 0'. ->
            get_code(Stream, _PC),         % Get the period
            get_decimal_digit_codes(FractionDigits1, Stream, Opts),
            (   FractionDigits1 = [_|_] -> % at least one digit must come after the period
                FractionDigits = FractionDigits1
            ;   peek_code(Stream, Lookahead),
                (   Lookahead == -1 ->
                    get_code(Stream, _),   % Get the EOF
                    throw_unexpected_end_of_file(Stream, Opts)
                ;   otherwise ->           % period followed by something else, leave it in the stream
                    throw_number_syntax_error(Lookahead, Stream, Opts)
                )
            )
        ;   otherwise ->                   % No fractional part
            FractionDigits = []
        ).


% "exp = e [ minus / plus ] 1*DIGIT"
% "e = %x65 / %x45            ; e E"
read_number_optional_exponent(Exponent, Stream, Opts) :-
        peek_code(Stream, PC),
        (   (   PC == 0'e
            ;   PC == 0'E
            ) ->
            get_code(Stream, _),          % get the e
            peek_code(Stream, PC1),
            (   PC1 == 0'- ->
                get_code(Stream, _PC1),
                ExpSign = 0'-
            ;   PC1 == 0'+ ->
                get_code(Stream, _PC1),
                ExpSign = 0'+
            ;   otherwise ->
                ExpSign = 0'+
            ),
            get_decimal_digit_codes(ExponentDigits, Stream, Opts),
            (   ExponentDigits = [_|_] -> % Exponent must have at least one digit
                Exponent = [0'e,ExpSign|ExponentDigits]
            ;   peek_code(Stream, Lookahead),
                (   Lookahead == -1 ->
                    get_code(Stream, _),  % Get the EOF
                    throw_unexpected_end_of_file(Stream, Opts)
                ;   otherwise ->          % followed by something else, leave it in the stream
                    throw_number_syntax_error(Lookahead, Stream, Opts)
                )
            )
        ;   otherwise ->                  % No exponent part
            Exponent = []
        ).


% ################


% Get and return zero or more digits.
% Consume all available digits ("[0-9]*]). Leaves the following code (which may be EOF) in the stream, unconsumed.
get_decimal_digit_codes(Digits, Stream, Opts) :-
        peek_code(Stream, PC),
        get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts).


get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'0,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'1,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'2,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'3,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'4,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'5,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'6,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'7,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'8,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(PC, Digits, Stream, Opts) :-
        PC = 0'9,
        !,
        get_decimal_digit_codes_dispatch_continue(PC, Digits, Stream, Opts).
get_decimal_digit_codes_dispatch(_PC, Digits, _Stream, _Opts) :-
        Digits = [].


get_decimal_digit_codes_dispatch_continue(PC, [PC|Digits], Stream, Opts) :-
        get_code(Stream, _PC),
        get_decimal_digit_codes(Digits, Stream, Opts).


% Get codes from streams matching all but the first of Expected
% Throws exceptions for unexpected input.
expect_remaining_codes(Expected, Stream, Opts) :-
        Expected = [_|Remaining],
        expect_codes(Remaining, Expected, Stream, Opts).

expect_codes([], _Expected, _Stream, _Opts).
expect_codes([C|Cs], Expected, Stream, Opts) :-
        get_code(Stream, Next),
        (   Next == C ->
            expect_codes(Cs, Expected, Stream, Opts)
        ;   Next == -1 ->
            throw_unexpected_end_of_file(Stream, Opts)
        ;   otherwise ->
            throw_literal_syntax_error(Next, [C|Cs], Expected, Stream, Opts)
        ).

:- throw_number_syntax_error/3 is throwing.
throw_number_syntax_error(_C, _Stream, _Opts) :-
        throw(error(syntax_error(json(illegal_number_syntax)),_)).

:- throw_syntax_error/2 is throwing.
throw_syntax_error(_Stream, _Opts) :-
        throw(error(syntax_error(json(illegal_json)),_)).


:- throw_literal_syntax_error/5 is throwing.
throw_literal_syntax_error(_C, _RemainingExpected, _Expected, Stream, Opts) :-
        throw_syntax_error(Stream, Opts).

:- throw_unexpected_end_of_file/2 is throwing.
throw_unexpected_end_of_file(_Stream, _Opts) :-
        throw(error(syntax_error(json(unexpected_end_of_file)),_)).

:- throw_escape_sequence_syntax_error/3 is throwing.
throw_escape_sequence_syntax_error(_C, _Stream, _Opts) :-
        throw(error(syntax_error(json(illegal_string_escape)),_)).

:- throw_string_syntax_error/3 is throwing.
throw_string_syntax_error(_C, _Stream, _Opts) :-
        throw(error(syntax_error(json(illegal_string_syntax)),_)).

:- throw_option_error/2 is throwing.
throw_option_error(_Options, _Goal) :-
        throw(error(json(invalid_options),_)).


:- throw_json_type_error/2 is throwing.
throw_json_type_error(Term, _Goal) :-
        throw_json_type_error(Term).


:- throw_json_type_error/1 is throwing.
throw_json_type_error(Term) :-
        throw(error(type_error(json_term,Term),_)).


% Has read <quotation-mark> but nothing else
read_string_continue(String, Stream, Opts) :-
        read_string_codes_continue(Codes, Stream, Opts),
        atom_codes(String, Codes).

% Inside string
read_string_codes_continue(Codes, Stream, Opts) :-
        get_code(Stream, C),
        read_string_tail(C, Codes, Stream, Opts).

read_string_tail(-1, _Codes, Stream, Opts) :-
        !,
        throw_unexpected_end_of_file(Stream, Opts).
read_string_tail(0'", Codes, _Stream, _Opts) :-
        !,
        Codes = [].
read_string_tail(0'\\, Codes, Stream, Opts) :-
        !,
        get_code(Stream, C),
        read_string_reverse_solidus_tail(C, Codes, Stream, Opts).
read_string_tail(C, Codes, Stream, Opts) :-
        % Control characters are not allowed (this incudes non-SPACE whitespace characters)
        0x001F < C,
        % Surrogates: 0xD800..0xDFFF.
        (   C < 0xD800 ->       % Below Low surrogate. The expected case.
            true
        ;   0xDFFF < C          % Above High surrogate. Valid but less common.
        ),
        !,
        Codes = [C|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_tail(C, Codes, Stream, Opts) :-
        % C is control or surrogate.
        handle_read_invalid_string_char(C, Stream, Opts),
        Codes = [C|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).


:- handle_read_invalid_string_char/3 is throwing.
% Quietly succeed if Opts allows invalid chars in strings.
% TODO: Implement
handle_read_invalid_string_char(C, Stream, Opts) :-
        % Control char
        C =< 0x001F,
        !,
        throw_string_syntax_error(C, Stream, Opts).
handle_read_invalid_string_char(C, Stream, Opts) :-
        % Surrogate
        0xD800 =< C,
        C =< 0xDFFF,
        !,
        throw_string_syntax_error(C, Stream, Opts).


% Have read reverse solidus (\) in  string
/*
   \" represents the quotation mark character (U+0022).
   \\ represents the reverse solidus character (U+005C).
   \/ represents the solidus character (U+002F).
   \b represents the backspace character (U+0008).
   \f represents the form feed character (U+000C).
   \n represents the line feed character (U+000A).
   \r represents the carriage return character (U+000D).
   \t represents the character tabulation character (U+0009).
 */
read_string_reverse_solidus_tail(-1, _Codes, Stream, Opts) :-
        !,
        throw_unexpected_end_of_file(Stream, Opts).
read_string_reverse_solidus_tail(0'u, Codes, Stream, Opts) :-
        !,
        read_string_first_utf16_continue(Codes, Stream, Opts).
read_string_reverse_solidus_tail(0'", Codes, Stream, Opts) :-
        !,
        Codes = [0x0022|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(0'\\, Codes, Stream, Opts) :-
        !,
        Codes = [0x005C|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(0'/, Codes, Stream, Opts) :-
        !,
        Codes = [0x002F|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(0'b, Codes, Stream, Opts) :-
        !,
        Codes = [0x0008|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(0'f, Codes, Stream, Opts) :-
        !,
        Codes = [0x000C|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(0'n, Codes, Stream, Opts) :-
        !,
        Codes = [0x000A|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(0'r, Codes, Stream, Opts) :-
        !,
        Codes = [0x000D|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(0't, Codes, Stream, Opts) :-
        !,
        Codes = [0x0009|Codes1],
        read_string_codes_continue(Codes1, Stream, Opts).
read_string_reverse_solidus_tail(C, _Codes, Stream, Opts) :-
        !,
        throw_escape_sequence_syntax_error(C, Stream, Opts).

% Have seen \u
% May need to read a following low surrogate
read_string_first_utf16_continue(Codes, Stream, Opts) :-
        read_bmp_code_continue(FirstCode, Stream, Opts),
        read_string_first_utf16_continue_1(FirstCode, Codes, Stream, Opts).

% Read a BMP code, as four hex digits
read_bmp_code_continue(Code, Stream, Opts) :-
        get_code(Stream, C1),
        parse_hex_digit(C1, D1, Stream, Opts),
        get_code(Stream, C2),
        parse_hex_digit(C2, D2, Stream, Opts),
        get_code(Stream, C3),
        parse_hex_digit(C3, D3, Stream, Opts),
        get_code(Stream, C4),
        parse_hex_digit(C4, D4, Stream, Opts),
        Code is (((((D1<<4)\/D2)<<4)\/D3)<<4)\/D4.

parse_hex_digit(-1, _D, Stream, Opts) :-
        !,
        throw_unexpected_end_of_file(Stream, Opts).
parse_hex_digit(0'0, D, _Stream, _Opts) :-
        !,
        D = 0.
parse_hex_digit(0'1, D, _Stream, _Opts) :-
        !,
        D = 1.
parse_hex_digit(0'2, D, _Stream, _Opts) :-
        !,
        D = 2.
parse_hex_digit(0'3, D, _Stream, _Opts) :-
        !,
        D = 3.
parse_hex_digit(0'4, D, _Stream, _Opts) :-
        !,
        D = 4.
parse_hex_digit(0'5, D, _Stream, _Opts) :-
        !,
        D = 5.
parse_hex_digit(0'6, D, _Stream, _Opts) :-
        !,
        D = 6.
parse_hex_digit(0'7, D, _Stream, _Opts) :-
        !,
        D = 7.
parse_hex_digit(0'8, D, _Stream, _Opts) :-
        !,
        D = 8.
parse_hex_digit(0'9, D, _Stream, _Opts) :-
        !,
        D = 9.
parse_hex_digit(0'a, D, _Stream, _Opts) :-
        !,
        D = 0xa.
parse_hex_digit(0'b, D, _Stream, _Opts) :-
        !,
        D = 0xb.
parse_hex_digit(0'c, D, _Stream, _Opts) :-
        !,
        D = 0xc.
parse_hex_digit(0'd, D, _Stream, _Opts) :-
        !,
        D = 0xd.
parse_hex_digit(0'e, D, _Stream, _Opts) :-
        !,
        D = 0xe.
parse_hex_digit(0'f, D, _Stream, _Opts) :-
        !,
        D = 0xf.
parse_hex_digit(0'A, D, _Stream, _Opts) :-
        !,
        D = 0xA.
parse_hex_digit(0'B, D, _Stream, _Opts) :-
        !,
        D = 0xB.
parse_hex_digit(0'C, D, _Stream, _Opts) :-
        !,
        D = 0xC.
parse_hex_digit(0'D, D, _Stream, _Opts) :-
        !,
        D = 0xD.
parse_hex_digit(0'E, D, _Stream, _Opts) :-
        !,
        D = 0xE.
parse_hex_digit(0'F, D, _Stream, _Opts) :-
        !,
        D = 0xF.
parse_hex_digit(C, _D, Stream, Opts) :-
        throw_escape_sequence_syntax_error(C, Stream, Opts).


read_string_first_utf16_continue_1(FirstCode, Codes, Stream, Opts) :-
        (   0xD800 =< FirstCode,
            FirstCode =< 0xDBFF -> % High surrogate
            get_code(Stream, Lookahead1),
            read_string_after_high_surrogate(Lookahead1, Codes, Stream, Opts, FirstCode)
        ;   0xDC00 =< FirstCode,
            FirstCode =< 0xDFFF -> % Lone Low surrogate
            handle_read_lone_escaped_surrogate(FirstCode, Stream, Opts),
            Codes = [FirstCode|Codes1],
            read_string_codes_continue(Codes1, Stream, Opts)
        ;   otherwise ->
            Codes = [FirstCode|Codes1],
            read_string_codes_continue(Codes1, Stream, Opts)
        ).


% Have read high surrogate, look ahead for the low surrogate, if present.
read_string_after_high_surrogate(0'\\, Codes, Stream, Opts, FirstCode) :-
        !,
        get_code(Stream, Lookahead2),
        (   Lookahead2 = 0'u ->
            read_bmp_code_continue(SecondCode, Stream, Opts),
            (   0xDC00 =< SecondCode,
                SecondCode =< 0xDFFF -> % Matching Low Surrogate
                decode_surrogate_pair(FirstCode, SecondCode, Code),
                Codes = [Code|Codes1],
                read_string_codes_continue(Codes1, Stream, Opts)
            ;   otherwise ->            % FirstCode is Lone High surrogate
                handle_read_lone_escaped_surrogate(FirstCode, Stream, Opts),
                Codes = [FirstCode|Codes1],
                % SecondCode can be a High surrogate that has a matching Low surrogate.
                read_string_first_utf16_continue_1(SecondCode, Codes1, Stream, Opts)
            )
        ;   otherwise ->
            % \Lookahead2 where Lookahead2 is not u, Code1 was a lone High surrogate code.
            handle_read_lone_escaped_surrogate(FirstCode, Stream, Opts),
            Codes = [FirstCode|Codes1],
            read_string_reverse_solidus_tail(Lookahead2, Codes1, Stream, Opts)
        ).
read_string_after_high_surrogate(Lookahead, Codes, Stream, Opts, FirstCode) :-
        % \uXXXX<Lookahead> where Lookahead is not reverse solidus, FirstCode is a lone High surrogate code XXXX.
        handle_read_lone_escaped_surrogate(FirstCode, Stream, Opts),
        Codes = [FirstCode|Codes1],
        read_string_tail(Lookahead, Codes1, Stream, Opts).


handle_read_lone_escaped_surrogate(_SurrogateCode, _Stream, _Opts) :-
        % TODO: Could optionally throw an error
        true.


decode_surrogate_pair(HighSurrogate, LowSurrogate, Code) :-
        %% "0x010000 is subtracted from the code point, leaving a 20-bit number in the range 0x000000..0x0FFFFF.
        %% The top ten bits (a number in the range 0x0000..0x03FF) are added to 0xD800 to give the first 16-bit
        %% code unit or high surrogate, which will be in the range 0xD800..0xDBFF.
        %% The low ten bits (also in the range 0x0000..0x03FF) are added to 0xDC00 to give
        %% the second 16-bit code unit or low surrogate, which will be in the range 0xDC00..0xDFFF."
        TopTenBits is HighSurrogate-0xD800,
        LowTenBits is LowSurrogate-0xDC00,
        Code is 0x010000+(TopTenBits<<10\/LowTenBits).


% Get next code that is not whitespace. Will return -1 for EOF
get_non_whitespace_code(Stream, Code) :-
        get_code(Stream, C),
        get_non_whitespace_code_dispatch(C, Stream, Code).


get_non_whitespace_code_dispatch(C, Stream, Code) :-
        C == 0x0009,            % character tabulation (U+0009).
        !,
        get_non_whitespace_code(Stream, Code).
get_non_whitespace_code_dispatch(C, Stream, Code) :-
        C == 0x000A,            % line feed (U+000A).
        !,
        get_non_whitespace_code(Stream, Code).
get_non_whitespace_code_dispatch(C, Stream, Code) :-
        C == 0x000D,            % carriage return (U+000D)
        !,
        get_non_whitespace_code(Stream, Code).
get_non_whitespace_code_dispatch(C, Stream, Code) :-
        C == 0x0020,            % space (U+0020).
        !,
        get_non_whitespace_code(Stream, Code).
get_non_whitespace_code_dispatch(C, _Stream, Code) :-
        Code = C.

:- if(false).
% Use later, perhaps.
% Peek and return next code that is not whitespace. Will return -1 for EOF
peek_non_whitespace_code(Stream, Code) :-
        peek_code(Stream, PC),
        peek_non_whitespace_code_dispatch(PC, Stream, Code).


peek_non_whitespace_code_dispatch(PC, Stream, Code) :-
        PC == 0x0009,           % character tabulation (U+0009).
        !,
        get_code(Stream, _PC),  % discard it
        peek_non_whitespace_code(Stream, Code).
peek_non_whitespace_code_dispatch(PC, Stream, Code) :-
        PC == 0x000A,           % line feed (U+000A).
        !,
        get_code(Stream, _PC),
        peek_non_whitespace_code(Stream, Code).
peek_non_whitespace_code_dispatch(PC, Stream, Code) :-
        PC == 0x000D,           % carriage return (U+000D)
        !,
        get_code(Stream, _PC),
        peek_non_whitespace_code(Stream, Code).
peek_non_whitespace_code_dispatch(PC, Stream, Code) :-
        PC == 0x0020,           % space (U+0020).
        !,
        get_code(Stream, _PC),
        peek_non_whitespace_code(Stream, Code).
peek_non_whitespace_code_dispatch(PC, _Stream, Code) :-
        Code = PC.
:- endif.

:- if(false).
% Use later, perhaps.
% Discard whitespace (but not EOF)
skip_whitespace(Stream) :-
        peek_code(Stream, PC),
        skip_whitespace_dispatch(PC, Stream).


skip_whitespace_dispatch(PC, Stream) :-
        PC == 0x0009,           % character tabulation (U+0009).
        !,
        get_code(Stream, _PC),  % discard it
        skip_whitespace(Stream).
skip_whitespace_dispatch(PC, Stream) :-
        PC == 0x000A,           % line feed (U+000A).
        !,
        get_code(Stream, _PC),
        skip_whitespace(Stream).
skip_whitespace_dispatch(PC, Stream) :-
        PC == 0x000D,           % carriage return (U+000D)
        !,
        get_code(Stream, _PC),
        skip_whitespace(Stream).
skip_whitespace_dispatch(PC, Stream) :-
        PC == 0x0020,           % space (U+0020).
        !,
        get_code(Stream, _PC),
        skip_whitespace(Stream).
skip_whitespace_dispatch(_PC, _Stream).
:- endif.


is_value(Value, Opts) :-
        opt_value(null, Opts, NullTerm),
        Value == NullTerm,
        !,
        true.
is_value(Value, Opts) :-
        opt_value(true, Opts, TrueTerm),
        Value == TrueTerm,
        !,
        true.
is_value(Value, Opts) :-
        opt_value(false, Opts, FalseTerm),
        Value == FalseTerm,
        !,
        true.
is_value(Var, _Opts) :-
        var(Var),
        !,
        fail.
is_value(json(NameValueList), Opts) :-
        opt_value(object_tag, Opts, Tag),
        Tag == json,
        !,
        is_list(NameValueList),
        is_object(NameValueList, Opts).
is_value(Array, Opts) :-
        Array = [],
        opt_value(array_tag, Opts, Tag),
        var(Tag),
        !,
        true.
is_value(NameValueList, Opts) :-
        NameValueList = [],
        opt_value(object_tag, Opts, Tag),
        var(Tag),
        !,
        true.
is_value(NameValueList, Opts) :-
        NameValueList = [_|_],
        opt_value(object_tag, Opts, Tag),
        var(Tag),
        !,
        is_list(NameValueList),
        is_object(NameValueList, Opts).
is_value(Array, Opts) :-
        Array = [_|_],
        opt_value(array_tag, Opts, Tag),
        var(Tag),
        !,
        is_list(Array),
        is_array(Array, Opts).
is_value(Value, Opts) :-
        functor(Value, Tag, 1),
        opt_value(object_tag, Opts, ObjectTag),
        Tag == ObjectTag,
        !,
        arg(1, Value, NameValueList),
        is_list(NameValueList),
        is_object(NameValueList, Opts).
is_value(Value, Opts) :-
        functor(Value, Tag, 1),
        opt_value(array_tag, Opts, ObjectTag),
        Tag == ObjectTag,
        !,
        arg(1, Value, Array),
        is_list(Array),
        is_array(Array, Opts).
is_value(Number, _Opts) :-
        number(Number),
        !,
        true.
is_value(String, _Opts) :-
        atom(String),
        !,
        true.

% Caller ensures is_list/1.
is_array([], _Opts).
is_array([V|Vs], Opts) :-
        is_value(V, Opts),
        is_array(Vs, Opts).

% Caller ensures is_list/1.
is_object([], _Opts).
is_object([V|Vs], Opts) :-
        is_key_value(V, Opts),
        is_object(Vs, Opts).

is_key_value(V, _Opts) :-
        var(V),
        !,
        fail.
is_key_value(K=V, Opts) :-
        atom(K),
        is_value(V, Opts).

% "A JSON value can be an object, array, number, string, true, false, or null"
write_value(Value, Stream, Opts) :-
        opt_value(null, Opts, NullTerm),
        Value == NullTerm,
        !,
        write(Stream, null).
write_value(Value, Stream, Opts) :-
        opt_value(true, Opts, TrueTerm),
        Value == TrueTerm,
        !,
        write(Stream, true).
write_value(Value, Stream, Opts) :-
        opt_value(false, Opts, FalseTerm),
        Value == FalseTerm,
        !,
        write(Stream, false).

write_value(Var, Stream, Opts) :-
        var(Var),
        !,
        write_other(Var, Stream, Opts).
write_value(Array, Stream, Opts) :-
        Array = [],
        opt_value(array_tag, Opts, ATag),
        var(ATag),
        !,
        write_array(Array, Stream, Opts).
write_value(NameValueList, Stream, Opts) :-
        NameValueList = [],
        opt_value(object_tag, Opts, OTag),
        var(OTag),
        !,
        write_object(NameValueList, Stream, Opts).
write_value(Array, Stream, Opts) :-
        Array = [_|_],
        opt_value(array_tag, Opts, ATag),
        var(ATag),
        !,
        write_array(Array, Stream, Opts).
write_value(NameValueList, Stream, Opts) :-
        NameValueList = [_|_],
        opt_value(object_tag, Opts, OTag),
        var(OTag),
        !,
        write_object(NameValueList, Stream, Opts).
write_value(json(NameValueList), Stream, Opts) :-
        opt_value(object_tag, Opts, OTag),
        OTag == json,
        !,
        write_object(NameValueList, Stream, Opts).
write_value(Object, Stream, Opts) :-
        compound(Object),
        functor(Object, F, 1),
        opt_value(object_tag, Opts, OTag),
        OTag == F,
        !,
        arg(1, Object, NameValueList),
        write_object(NameValueList, Stream, Opts).
write_value(Array, Stream, Opts) :-
        compound(Array),
        functor(Array, F, 1),
        opt_value(array_tag, Opts, ATag),
        ATag == F,
        !,
        arg(1, Array, Elements),
        write_array(Elements, Stream, Opts).
write_value(Number, Stream, Opts) :-
        number(Number),
        !,
        write_number(Number, Stream, Opts).
write_value(String, Stream, Opts) :-
        atom(String),
        !,
        write_string(String, Stream, Opts).
write_value(Other, Stream, Opts) :-
        write_other(Other, Stream, Opts).

:- write_other/3 is throwing.

write_other(Other, _Stream, _Opts) :-
        var(Other),
        !,
        throw(error(instantiation_error,_)).
write_other(Other, _Stream, _Opts) :-
        throw_json_type_error(Other).


write_string(String, Stream, Opts) :-
        atom_codes(String, Codes),
        write(Stream, '"'),
        write_string_codes(Codes, Stream, Opts),
        write(Stream, '"').

write_string_codes([], _Stream, _Opts) :-
        true.
write_string_codes([Code|Codes], Stream, Opts) :-
        write_string_codes1(Code, Codes, Stream, Opts).

% write_string_codes(Codes,  Stream,Opts).
% "the characters that MUST be escaped: quotation mark, reverse solidus, and the control characters (U+0000 through U+001F)"
write_string_codes1(0'", Codes, Stream, Opts) :-
        !,
        write(Stream, '\\"'),
        write_string_codes(Codes, Stream, Opts).
write_string_codes1(0'\\, Codes, Stream, Opts) :-
        !,
        write(Stream, '\\\\'),
        write_string_codes(Codes, Stream, Opts).
write_string_codes1(0'<, [0'/|Codes], Stream, Opts) :-
        !,
        % Make it optional?
        % Special case: "...</..." is instead written as "...<\/..." (for easier <script> embedding).
        write(Stream, '<\\/'),
        write_string_codes(Codes, Stream, Opts).
write_string_codes1(Control, Codes, Stream, Opts) :-
        % This also catches all non-space whitespace that must be quoted ("character tabulation (U+0009), line feed (U+000A), carriage return (U+000D)"
        Control =< 0x001F,
        !,
        write_string_control(Control, Stream, Opts),
        write_string_codes(Codes, Stream, Opts).
write_string_codes1(Code, Codes, Stream, Opts) :-
        Code =< 127,            % ASCII
        !,
        put_code(Stream, Code),
        write_string_codes(Codes, Stream, Opts).
write_string_codes1(Code, Codes, Stream, Opts) :-
        % Code >= 128,            % non-ASCII
        Code =< 0xFFFF,         % BMP
        opt_value(ascii, Opts, false),
        !,
        put_code(Stream, Code),
        write_string_codes(Codes, Stream, Opts).
write_string_codes1(Code, Codes, Stream, Opts) :-
        % Must be escaped
        write_string_hex(Code, Stream, Opts),
        write_string_codes(Codes, Stream, Opts).


write_string_control(Var, _Stream, _Opts) :-
        var(Var),
        !,
        fail.
write_string_control(C, Stream, Opts) :-
        string_escaped_name(C, Name, Stream, Opts),
        !,
        write(Stream, '\\'),
        write(Stream, Name).
write_string_control(C, Stream, Opts) :-
        write_string_hex(C, Stream, Opts).

write_string_hex(C, Stream, _Opts) :-
        C =< 0xFFFF,            % BMP
        !,
        write_bmp_code(C, Stream).
write_string_hex(C, Stream, _Opts) :-
        0xFFFF < C,
        C =< 0x10FFFF,          % supplemental
        !,
        %% "0x010000 is subtracted from the code point, leaving a 20-bit number in the range 0x000000..0x0FFFFF.
        %% The top ten bits (a number in the range 0x0000..0x03FF) are added to 0xD800 to give the first 16-bit
        %% code unit or high surrogate, which will be in the range 0xD800..0xDBFF.
        %% The low ten bits (also in the range 0x0000..0x03FF) are added to 0xDC00 to give
        %% the second 16-bit code unit or low surrogate, which will be in the range 0xDC00..0xDFFF."
        Bits is C-0x10000,
        TopTenBits is Bits>>10,
        LowTenBits is Bits/\0x03FF,
        HighSurrogate is 0xD800+TopTenBits,
        LowSurrogate is 0xDC00+LowTenBits,
        write_bmp_code(HighSurrogate, Stream),
        write_bmp_code(LowSurrogate, Stream).


write_bmp_code(C, Stream) :-
        % \uXXXX zero-padded to the left.
        format(Stream, '\\u~|~`0t~16R~4+', [C]).


string_escaped_name(0x0022, '"', _Stream, _Opts). % quotation mark  U+0022
string_escaped_name(0x005C, '\\', _Stream, _Opts). % /          ; \    reverse solidus U+005C
string_escaped_name(0x002F, '/', _Stream, _Opts). % solidus         U+002F
string_escaped_name(0x0008, b, _Stream, _Opts). % backspace       U+0008
string_escaped_name(0x000C, f, _Stream, _Opts). % form feed       U+000C
string_escaped_name(0x000A, n, _Stream, _Opts). % line feed       U+000A
string_escaped_name(0x000D, r, _Stream, _Opts). % carriage return U+000D
string_escaped_name(0x0009, t, _Stream, _Opts). % tab             U+0009


write_number(Number, Stream, _Opts) :-
        write_canonical(Stream, Number).


write_object(Members, Stream, Opts) :-
        write(Stream, '{'),
        write_layout('\n', Stream, Opts),
        write_object_members(Members, Stream, Opts),
        write_layout('\n', Stream, Opts),
        write(Stream, '}').

% Write optional layout-related characters. Does nothing if Opts specifies compact format.
write_layout(_Text, _Stream, Opts) :-
        opt_value(compact, Opts, true),
        !.
write_layout(Text, Stream, _Opts) :-
        write(Stream, Text).

write_object_members([], _Stream, _Opts).
write_object_members([Member|Members], Stream, Opts) :-
        write_layout('  ', Stream, Opts),
        write_object_member(Member, Stream, Opts),
        write_object_member_suffix(Members, Stream, Opts),
        write_object_members(Members, Stream, Opts).

write_object_member(Member, _Stream, _Opts) :-
        var(Member),
        !,
        throw(error(instantiation_error,_)).
write_object_member(Name=Value, Stream, Opts) :-
        !,
        write_object_member(Name, Value, Stream, Opts).
% Compatibility: Handle (Name-Value) as(Name=Value)
write_object_member(Name-Value, Stream, Opts) :-
        !,
        write_object_member(Name, Value, Stream, Opts).
% Compatibility: Handle Name(Value) as(Name=Value)
write_object_member(NameValue, Stream, Opts) :-
        compound(NameValue),
        functor(NameValue, Name, 1),
        !,
        arg(1, NameValue, Value),
        write_object_member(Name, Value, Stream, Opts).
write_object_member(_NameValue, _Stream, _Opts) :-
        fail.

write_object_member(Name, Value, Stream, Opts) :-
        write_string(Name, Stream, Opts),
        write(Stream, ':'),
        write_value(Value, Stream, Opts).

write_object_member_suffix([], _Stream, _Opts) :-
        % No more members.
        true.
write_object_member_suffix([_|_], Stream, Opts) :-
        % More members.
        write(Stream, ','),
        write_layout('\n', Stream, Opts).

% "An array structure is a pair of square bracket tokens surrounding zero or more values. The values are separated by commas"
write_array(Array, Stream, Opts) :-
        write(Stream, '['),
        write_array_members(Array, Stream, Opts),
        write(Stream, ']').


write_array_members([], _Stream, _Opts).
write_array_members([Member|Members], Stream, Opts) :-
        write_array_member(Member, Stream, Opts),
        write_array_member_suffix(Members, Stream, Opts),
        write_array_members(Members, Stream, Opts).


write_array_member(Member, Stream, Opts) :-
        write_value(Member, Stream, Opts).


write_array_member_suffix([], _Stream, _Opts) :-
        % No more members.
        true.
write_array_member_suffix([_|_], Stream, Opts) :-
        % More members.
        write(Stream, ','),
        write_layout(' ', Stream, Opts).

%@  @end table @c Exported Predicates
