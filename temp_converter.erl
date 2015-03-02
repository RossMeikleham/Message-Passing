% Temperature Conversion message passing system
% Ross Meikleham, 2015

-module(temp_sensors).

-export([start/0]).

% Display a temperature
display() ->
    receive
        {From, message} ->
            io:fwrite(message),
            display()
    end.

% Convert Celcius/Farenheit temperatures
temperature_converter() ->
    receive  
        % Farenheit to Celcius
        {convertToCelsius, Celcius_PID, T} -> 
            Celcius_PID ! {temperature, T, ((9/5) * T + 32)}; 

        % Celcius to Farenheit
        {convertToFarenheit, Farenheit_PID, T} ->    
            Farenheit_PID ! {temperature, T, (T - 32) * (5/9)}
     end,
     temperature_converter()

% Sends a timer message to celcius and farenheit processes
% every second
clock(Celcius_PID, Farenheit_PID) ->
%    Celcius_Ref = erlang:monitor(celcius_sensor, Celcius_PID),
%    Farenheit_Ref = erlang:monitor(farenheit_sensor, Farenheit_PID),
    timer:sleep(1000),
    Celcius_PID ! timer,
%    Farenheit_PID ! timer,
    clock(CELCIUS_PID, FARENHEIT_PID).

%Sensor which reads temperatures in Celcius
celcius_sensor(Temps, Display_PID, Temp_Conv_PID) ->
    receive
        %Temperature converted, send message to display
        {temperature, C, F} -> Display_PID ! 
            io:format("Converted ~p degrees celcius to ~p degrees farenheit", [C, F]),
            celcius_sensor(Temps, Display_PID, Temp_Conv_PID);
        
        %Send head temperature to converter
        timer -> case Temps 
                   % If we have temperatures to send, send the first
                   [Temp | RestTemps] ->  
                      Temp_Conv_PID ! {self(), Temp},
                      celcius_sensor(RestTemps, Display_PID, Temp_Conv_PID);

                    [] ->
                      celcius_sensor([], Display_PID, Temp_Conv_PID)
                 end
    end.


%Sensor which rads temperatures in Farenheit
farenheit_sensor(Temps , Display_PID, Temp_Conv_PID) ->
    receive
        %Temperature converted, send message to display
        {temperature, F, C} -> Display_PID !
            io:format("Converted ~p degrees farenheit to ~p degrees celcius.~n", [F, C]),
            farenheit_sensor([Temp | RestTemps], Display_PID, Temp_Conv_PID)
        
        % Send head temperature to converter    
        timer -> Temp_Conv_PID ! {self(), Temps},
                 farenheit_sensor(RestTemps, Display_PID, Temp_Conv_PID)
     end.


start() ->
    Display_PID = spawn(temp_sensors, display)
    Temp_Conv_PID = spawn(temp_sensors, temperature_converter)
    Farenheit_PID = spawn(temp_sensors, farenheit_sensor, [[10, 20, 30], Display_PID, Temp_Conv_PID]),
    Celcius_PID = spawn(temp_sensors, celcius_sensor, [[40, 50, 60], Display_PID, Temp_Conv_PID]),
    Clock_PID = spawn(temp_sensors, clock, [Farenheit_PID, Celcius_PID]).

