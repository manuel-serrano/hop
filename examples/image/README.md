This example illustrates standard client image manipulations and use
of `hop.HTTPResponseProxy` objects.

Security enforcement of Web browsers prevent manipulation image pixels whose
origin differs from the origin of the main page. To workaround this problem,
the manipulated image is _proxied_ by the Hop server. The Web browser only
_sees_ relative local URLs.
