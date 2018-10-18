This example shows how to use WRP (Web Reactive Programming) to
automatically update client interfaces upon server updates.

A reactor is created from the the server event `clients`. Each time
the server broadcast this event, _i.e._, on each new connection,
all the already connected clients are updated to reflect the number
of connected clients and their connection times.
