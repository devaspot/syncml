-module(wbxml).
-export([xml/1]).

xml(Content) ->
    wbxml_encoder:xml(Content).
