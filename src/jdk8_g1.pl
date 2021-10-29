:- module( jdk8_g1, [ parse_g1_file/1 ] ).

:- use_module(library(lists)).

:- use_module(log_types).

%
parse_g1_file(File) :-
    open(File, read, Stream, [encoding('UTF-8')]),
    see(Stream),
    parse_g1,
    seen.
        
parse_g1 :- at_end_of_stream.
parse_g1 :-
    get_line([], Line), !,
    parse(Line),
    parse_g1.

get_line(Line, Line) :- at_end_of_stream, !.
get_line(Line, Line) :- at_end_of_line, !, get_code(_).
get_line(T, [H|NewT]) :-
    get_code(H),
    get_line(T, NewT).

print_gc(gc(Time, _TimeStamp, Type, Pause, Others, sizes(Heap, Eden, Survivor), _Times)) :-
    Heap = heap(before(size(_HeapBefore), capacity(_HeapCapacityBefore)), after(size(HeapAfter), capacity(_HeapCapacityAfter))),
    Eden = eden(before(size(EdenBefore), capacity(_EdenCapacityBefore)), after(size(_EdenAfter), capacity(_EdenCapacityAfter))),
    Survivor = survivor(before(capacity(_SurvivorBefore)), after(capacity(SurvivorAfter))),
    print(gc(Time, Type, Others, Pause, eden(EdenBefore), survivor(SurvivorAfter), heap(HeapAfter))).

parse(Line) :-
    (parse(GC, Line, _Res) ->
        print_gc(GC), nl
        ; true
    ).

parse(gc(Time, timestamp(TimeStamp), Type, pause(PauseTime), Others, Sizes, Times)) --> datetime(Time), ":", ws,
    decimal(TimeStamp), ": [", gc_header(Type),
    gc_header_more(PauseTime, Others),
    gc_heap_sizes_next_line(Sizes),
    gc_times_next_line(Times).

gc_header(type(full, Cause, [])) --> "Full GC (", gc_cause(Cause), ")".
gc_header(type(Type, Cause, Attr)) --> "GC pause (", gc_cause(Cause), ") ", gc_type(Type, Attr).

gc_type(young, [initial_marking]) --> "(young) (initial-mark)".
gc_type(young, []) --> "(young)".
gc_type(mixed, []) --> "(mixed)".

gc_cause(evacuation) --> "G1 Evacuation Pause".
gc_cause(gc_locker) --> "GCLocker Initiated GC".
gc_cause(humongous) --> "G1 Humongous Allocation".
gc_cause(allocation_failure) --> "Allocation Failure".

gc_header_more(PauseTime, []) --> ", ", decimal(PauseTime), " secs]".
gc_header_more(PauseTime, Others) --> "Before GC RS summary", gc_header_next_line(PauseTime, Others).
gc_header_more(PauseTime, Others) --> [], gc_header_next_line(PauseTime, Others).

gc_header_next_line(PauseTime, Others) --> { get_line([], Line), gc_header_multilines(PauseTime, Others, Line, _) }.

gc_header_multilines(PauseTime, [to-space-exhausted]) --> spaces, "(to-space exhausted), ", decimal(PauseTime), " secs]".
gc_header_multilines(PauseTime, []) --> ", ", decimal(PauseTime), " secs]".
gc_header_multilines(PauseTime, Others) --> gc_header_next_line(PauseTime, Others).

gc_heap_sizes_next_line(Sizes) --> { get_line([], Line), get_heap_sizes(Sizes, Line, _) }.

get_heap_sizes(sizes(Heap, Eden, Survivor)) --> 
    spaces, "[Eden: ", byte_size(EdenBefore), "(", byte_size(EdenCapacityBefore), ")->", 
    byte_size(EdenAfter), "(", byte_size(EdenCapacityAfter),
    ") Survivors: ", byte_size(SurvivorBefore), "->", byte_size(SurvivorAfter),
    " Heap: ", byte_size(HeapBefore), "(", byte_size(HeapCapacityBefore), ")->", 
    byte_size(HeapAfter), "(", byte_size(HeapCapacityAfter),
    {
        Heap = heap(before(size(HeapBefore), capacity(HeapCapacityBefore)), after(size(HeapAfter), capacity(HeapCapacityAfter))),
        Eden = eden(before(size(EdenBefore), capacity(EdenCapacityBefore)), after(size(EdenAfter), capacity(EdenCapacityAfter))),
        Survivor = survivor(before(capacity(SurvivorBefore)), after(capacity(SurvivorAfter)))
    }.
get_heap_sizes(Sizes) --> gc_heap_sizes_next_line(Sizes).

gc_times_next_line(Times) --> { get_line([], Line), gc_times(Times, Line, _) }.

gc_times(times(user(User), sys(Sys), real(Real))) --> spaces, "[Times: user=", decimal(User), " sys=", decimal(Sys) ,", real=", decimal(Real), " secs]".
gc_times(Times) --> gc_times_next_line(Times).