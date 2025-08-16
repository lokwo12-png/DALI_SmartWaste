%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test Scenario: Smart Waste Management MAS
% File: simulation1.pl
% Purpose:
%   - Simulate SmartBins becoming full
%   - Trigger ControlCenter to dispatch GarbageTrucks
%   - Observe Logger capturing all events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dali)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST SCENARIOS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Scenario 1: SmartBin1 at Market Road is full
test_scenario1 :-
    tell(smartbin1, detect_full(market_road)).

% Scenario 2: SmartBin2 at Hospital Road is full
test_scenario2 :-
    tell(smartbin2, detect_full(hospital_road)).

% Scenario 3: SmartBin3 at School Road is full
test_scenario3 :-
    tell(smartbin3, detect_full(school_road)).

% Scenario 4: Multiple bins become full at once
test_scenario4 :-
    tell(smartbin1, detect_full(market_road)),
    tell(smartbin2, detect_full(hospital_road)),
    tell(smartbin3, detect_full(school_road)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RUN ALL TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_all :-
    nl, write('>>> Running Scenario 1'), nl,
    test_scenario1, sleep(2),

    nl, write('>>> Running Scenario 2'), nl,
    test_scenario2, sleep(2),

    nl, write('>>> Running Scenario 3'), nl,
    test_scenario3, sleep(2),

    nl, write('>>> Running Scenario 4 (multiple bins)'), nl,
    test_scenario4.
