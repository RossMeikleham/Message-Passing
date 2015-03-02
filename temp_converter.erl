% Temperature Conversion message passing system
% Ross Meikleham, 2015

-module(temp_sensors).

-export([start/0]).

% Display a temperature
display() ->
    {convertToCelcius, c f}

temperature_converter() ->
    receive 
        
        {convertToCelsius, Celcius_PID, T} -> 
        {convertToFarenheit, T} ->    

clock(CELCIUS_PID, FARENHEIT_PID) ->
    timer:sleep(1000),
    temperature_converter ! CELCIUS_PID,
    temperature_converter ! FARENHEIT_PID,
    clock(CELCIUS_PID, FARENHEIT_PID).

celcius_sensor([], Display_PID) ->

celcius_sensor(temps, Display_PID) ->
    receive
        {Converted, f, c} -> %Send message to display
        Timer -> %Send head temperature to converter

farenheit_sensor([], Display_PID) ->
    %Finished

farenheit_sensor(temps, Display_PID) ->
    receive
        {Converted, c , f} -> %Send message to display
        Timer -> %Send head temperature to converter

start() ->
    Display_PID = spawn(temp_sensors, display)
    Farenheit_PID = spawn(temp_sensors, farenheit_sensor, [10, 20, 30]),
    Celcius_PID = spawn(temp_sensors, celcius_sensor, [40, 50, 60]),
    Clock_PID = spawn(temp_sensors, clock, [Farenheit_PID, Celcius_PID]).
