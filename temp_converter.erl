% Temperature Conversion message passing system
% Ross Meikleham, 2015

-module(temp_converter).

-export([start/0, display/0, temperature_converter/0, celcius_sensor/3, 
         farenheit_sensor/3, clock/3]).

% Display a recieved message
display() ->
    receive
        Message ->
            io:fwrite(Message),
            display()
    end.


% Convert Celcius/Farenheit temperatures
temperature_converter() ->
    receive  
        % Farenheit to Celcius
        {convertToCelsius, Farenheit_PID, T} -> 
            Farenheit_PID ! {temperature, T, (T - 32) * (5/9)};

        % Celcius to Farenheit
        {convertToFarenheit, Celcius_PID, T} ->    
            Celcius_PID ! {temperature, T, ((9/5) * T + 32)}
     end,
     temperature_converter().


% Sends a timer message to celcius and farenheit processes
% every second
clock(Celcius_PID, Farenheit_PID, Time) ->
    if Time > 0 -> 
        timer:sleep(1000),
        Celcius_PID ! timer,
        Farenheit_PID ! timer,
        clock(Celcius_PID, Farenheit_PID, Time - 1000);
        
        true -> {}
     end.


%Sensor which reads temperatures in Celcius
celcius_sensor(Temps, Display_PID, Temp_Conv_PID) ->
    receive
        %Temperature converted, send message to display
        {temperature, C, F} -> Display_PID ! 
            io_lib:format("Converted ~.2f degrees celcius to ~.2f degrees farenheit~n", [C, F]),
            celcius_sensor(Temps, Display_PID, Temp_Conv_PID);
        
        %Send head temperature to converter
        timer ->
                case Temps of 
                   % If we have temperatures to send, send the first
                   [Temp | RestTemps] ->  
                      Temp_Conv_PID ! {convertToFarenheit, self(), Temp},
                      celcius_sensor(RestTemps, Display_PID, Temp_Conv_PID);
                    % No temperatures to send
                    [] ->
                      celcius_sensor([], Display_PID, Temp_Conv_PID)
                 end
    end.


%Sensor which rads temperatures in Farenheit
farenheit_sensor(Temps , Display_PID, Temp_Conv_PID) ->
    receive
        %Temperature converted, send message to display
        {temperature, F, C} -> Display_PID !
            io_lib:format("Converted ~.2f degrees farenheit to ~.2f degrees celcius.~n", [F, C]),
            farenheit_sensor(Temps, Display_PID, Temp_Conv_PID);
        
        %Send temperature to converter
        timer ->
                case Temps of 
                   % If we have temperatures to send, send the first
                   [Temp | RestTemps] ->  
                      Temp_Conv_PID ! {convertToCelsius, self(), Temp}, 
                      farenheit_sensor(RestTemps, Display_PID, Temp_Conv_PID);
                    
                   % No temperatures to send
                   [] ->
                      farenheit_sensor([], Display_PID, Temp_Conv_PID)
                 end
     end.

% Montitor given temperature lists of floats, for a given amount of milliseconds
monitor_temps(Celcius_Temps, Farenheit_Temps, Time) ->
    io:fwrite("Starting temperature monitoring for ~pms~n", [Time]),

    Display_PID = spawn(temp_converter, display, []),
    Temp_Conv_PID = spawn(temp_converter, temperature_converter, []),
    Farenheit_PID = spawn(temp_converter, farenheit_sensor, [Farenheit_Temps, 
                          Display_PID, Temp_Conv_PID]),
    Celcius_PID = spawn(temp_converter, celcius_sensor, [Celcius_Temps, 
                        Display_PID, Temp_Conv_PID]),
    clock(Celcius_PID, Farenheit_PID, Time),

    % Clock has finished monitoring, exit rest of active processes
    exit(Display_PID, ok),
    exit(Temp_Conv_PID, ok),
    exit(Farenheit_PID, ok),
    exit(Celcius_PID, ok),

    io:fwrite("Finished temperature monitoring~n~n").


start() ->
    % Test monitoring of equal amounts of celcius/farenheit temps
    monitor_temps([10.0, 20.0, 35.0], [20.0, 35.0, 40.0], 8000),
    % -40 degrees farenheit should equal -40 degrees celcius
    monitor_temps([-40.0], [-40.0], 3000),
    % Check more farenheit temps than celcius works
    monitor_temps([-20.0], [200.0, 400.0, 500.0], 5000),
    % Check more celcius temps than farenheit works
    monitor_temps([-200.0, -50.0, 300.0], [-10.0, -20.0], 5000),
    % Check no celcius temps works
    monitor_temps([], [10.0, 20.0], 3000),
    %Check no farenheit temps works
    monitor_temps([143.27, 27.2, 33.3], [], 4000).  



