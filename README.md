Simple temperature monitoring written in Erlang.

                           +--------+
                     +-----| Sensor |<----+
                     |     +---+----+     |      +-------------+
    +---------+      |         |          +----->|             |
    |         |<-----+     +---+---+             | Temperature |
    | Display |            | Clock |             | Converter   |
    |         |<-----+     +---+---+      +----->|             |
    +---------+      |         |          |      +-------------+
                     |     +---+----+     |
                     +-----| Sensor |<----+
                           +--------+

# Running
 - Run the erlang shell with `erl`
 - `c(temp_converter)`
 -  to run tests enter `temp_converter:start()`
 -  to run your own temperatures, run `temp_converter:monitor_temps(celcius, faren, time)`
    celcius and faren should be lists of floats containing temperatures. time is the
    amount of time in millseconds to monitor for, if there is still time and all the
    temperatures in the lists have been converted then random temperatures will
    be generated and converted
