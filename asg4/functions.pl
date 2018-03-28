/*
 * Partner:  Jakrarin Srimakut (jsrimaku@ucsc.edu)
 * Partner:  Vivian Nguyen (vnguye77@ucsc.edu)
 */

/*
 * Function: Gets distance between airports using haversine formula.
 */

haversine_rads( Lat1, Lon1, Lat2, Lon2, Distance ) :-
     Dlon is Lon2 - Lon1,
     Dlat is Lat2 - Lat1,
     A is sin( Dlat / 2 ) ** 2
        + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
     Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
     Distance is Dist * 3961. 

degmin_in_radians( degmin( Degrees, Minutes ), Rads ) :-
     Degs is Degrees + Minutes / 60,
     Rads is Degs * pi / 180.

distances( Airport1, Airport2, Distance ) :-
     airport( Airport1, _, Lat1, Lon1 ),
     airport( Airport2, _, Lat2, Lon2 ),
     degmin_in_radians( Lat1, Lat1_float ),
     degmin_in_radians( Lat2, Lat2_float ),
     degmin_in_radians( Lon1, Lon1_float ),
     degmin_in_radians( Lon2, Lon2_float ),
     % write('Lat 1, Lat2, Lon1, Lon2 as floats: '), nl,
     % write(Lat1_float), nl, 
     % write(Lat2_float), nl, 
     % write(Lon1_float), nl, 
     % write(Lon2_float), nl, 
     haversine_rads( Lat1_float, Lon1_float, Lat2_float,
                            Lon2_float, Distance).
     % write('Distance: '), write(Distance),nl,

/*
 * Change the flight miles into time.
 * Plane flies at the constant speed of 500mph
 */

% Gets the time in hours.
hours_total( time( Hours, Mins ), Hourstot ) :-
     Hourstot is Hours + Mins / 60.

% Calculates the time in hous given 500mph.
hours_from_miles( Miles, Hours ) :-
     Hours is Miles / 500.

digsPrint( Timedigits ) :-
     Timedigits < 10, print( 0 ), print( Timedigits ).
digsPrint( Timedigits ) :-
     Timedigits >= 10, print( Timedigits ).
timePrint( Hourstot ) :-
     Minsdigits is floor( Hourstot * 60 ),
     Hours is Minsdigits // 60,
     Mins is Minsdigits mod 60,
     digsPrint( Hours ), print( ':' ), digsPrint( Mins ).

/*
 * The prolog version of not.
 */

not( X ) :- X, !, fail.
not( _ ).

/*
 * Creates a path between the departure and arrival airport. 
 * Flights must finish within the day (24:00) of the Twilight zone.
 * Flights have a 30 minute transfer time between other flights.
 * A list is returned with the shortest path.
 */

createFlight( Terminal, Terminal, _, [Terminal], _ ).
createFlight( Prev, Terminal, Visited, 
     [[Prev, FlightDep, FlightArr] | List], FlightDepInHM ) :-
     flight( Prev, Terminal, FlightDepInHM ),
     not( member( Terminal, Visited ) ),
     hours_total( FlightDepInHM, FlightDep ),
     distances( Prev, Terminal, FDistance ),
     hours_from_miles( FDistance, TimeDiff ),
     FlightArr is FlightDep + TimeDiff,
     FlightArr < 24.0,
     createFlight( Terminal, Terminal, [Terminal | Visited], List, _).
createFlight( Prev, Terminal, Visited, 
     [[Prev, FlightDep, FlightArr] | List], FlightDepInHM ) :-
     flight( Prev, Next, FlightDepInHM ),
     not( member( Next, Visited ) ),
     hours_total( FlightDepInHM, FlightDep ),
     distances( Prev, Next, FDistance ),
     hours_from_miles( FDistance, TimeDiff ),
     FlightArr is FlightDep + TimeDiff,
     FlightArr < 24.0,
     flight( Next, _, NextFlightDepInHM ),
     hours_total( NextFlightDepInHM, NextFlightDep ),
     AdjTime is NextFlightDep - FlightArr - 0.5,
     AdjTime >= 0,
     createFlight( Next, Terminal, [Next | Visited], 
          List, NextFlightDepInHM ).


% Write the flight list using a certain form given departs/arrives.

writePath( [] ) :-
     nl.
writePath( [[X, XDTime, XATime], Y | []] ) :-
     airport( X, Depart_Ext, _, _), airport( Y, Arrive_Ext, _, _),
     write( '      ' ), write( 'depart  ' ),
     write( X ), write( '  ' ),
     write( Depart_Ext ), timePrint( XDTime ), nl,
     write( '      ' ), write( 'arrive  ' ),
     write( Y ), write( '  ' ),
     write( Arrive_Ext ), timePrint( XATime ), nl,
     !, true.

writePath( [[X, XDTime, XATime], [Y, YDTime, YATime] | Z] ) :-
     airport( X, Depart_Ext, _, _), airport( Y, Arrive_Ext, _, _),
     write( '      ' ), write( 'depart  ' ),
     write( X ), write( '  ' ),
     write( Depart_Ext ), timePrint( XDTime ), nl,
     write( '      ' ), write( 'arrive  ' ),
     write( Y ), write( '  ' ),
     write( Arrive_Ext ), timePrint( XATime ), nl,
     !, writePath( [[Y, YDTime, YATime] | Z] ).

% Prints an error if flight destination and arrival are the same
flight( Depart, Depart ) :-
     write( 'Error: the departure and arrival of: ' ), write(Depart),
     write( ' to '), write(Depart), write( ' are the same.' ),
     nl,
     !, fail.

% Prints flight schedule with haversine formula
flight( Depart, Arrive ) :-
     airport( Depart, _, _, _ ),
     airport( Arrive, _, _, _ ),

     createFlight( Depart, Arrive, [Depart], List, _ ),
     !, nl,
     writePath( List ),
     true.

% Prints an error if flight path cannot be determined
flight( Depart, Arrive ) :-
     airport( Depart, _, _, _ ),
     airport( Arrive, _, _, _ ),
     write( 'Error: flight from: ' ), write(Depart),
     write( ' to '), write(Arrive), write( ' is not possible.' ),
     !, fail.

% Prints an error if airport cannot be found in database
flight( _, _) :-
     write( 'Error: nonexistent airports.' ), nl,
     !, fail.

