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
            Celcius_PID ! {T, ((9/5) * T + 32)} 

        {convertToFarenheit, Farenheit_PID, T} ->    
            Farenheit_PID ! {T, (T - 32) * (5/9)}

clock(CELCIUS_PID, FARENHEIT_PID) ->
    timer:sleep(1000),
    CELCIUS_PID ! self(),
    FARENHEIT_PID ! self(),
    clock(CELCIUS_PID, FARENHEIT_PID).

%Sensor which reads temperatures in celcius
celcius_sensor([], Display_PID, Temp_Conv_PID) ->

celcius_sensor([Temp | RestTemps], Display_PID, Temp_Conv_PID) ->
    receive
        %Temperature converted, send message to display
        {temperature_converter, C, F} -> Display_PID ! 
            io:format("Converted ~p degrees celcius to ~p degrees farenheit", [C, F])
        
        %Send head temperature to converter
        Timer -> Temp_Conv_PID ! {self(), Temp}
    end,
    celcius_sensor(RestTemps, Display_PID, Temp_Conv_PID) 

farenheit_sensor([], Display_PID, Temp_Conv_PID) ->
    %Finished

farenheit_sensor([HeadTemp | RestTemps] , Display_PID, Temp_Conv_PID) ->
    receive
        %Temperature converted, send message to display
        {temperature_converter, F, C} -> Display_PID !
            io:format("Converted ~p degrees farenheit to ~p degrees celcius", [F, C])
        
        % Send head temperature to converter    
        Timer -> 

start() ->
    Display_PID = spawn(temp_sensors, display)
    Temp_Conv_PID = spawn(temp_sensors, temperature_converter)
    Farenheit_PID = spawn(temp_sensors, farenheit_sensor, [[10, 20, 30], Display_PID, Temp_Conv_PID]),
    Celcius_PID = spawn(temp_sensors, celcius_sensor, [[40, 50, 60], Display_PID, Temp_Conv_PID]),
    Clock_PID = spawn(temp_sensors, clock, [Farenheit_PID, Celcius_PID]).

