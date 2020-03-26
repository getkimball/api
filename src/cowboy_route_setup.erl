-module(cowboy_route_setup).
-export([get_routes_from_modules/1]).


-callback setup() -> map().



get_routes_from_modules([]) ->
    [];
get_routes_from_modules([H|T]) ->
    ModSetup = H:setup(),
    Routes = get_routes_from_mod_setup(ModSetup),
    Routes ++ get_routes_from_modules(T).

get_routes_from_mod_setup(#{routes := Routes}) ->
    Routes.
