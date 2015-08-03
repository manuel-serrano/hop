This example shows the most efficient way to deliver content file
to client. Using a `HTTPResponseFile` object deliver much better
performance than reading the file content first and then seding a
buffer or a string the client.

In this example, the file replied to the client is to be interpreted
as a plain text file alhgouth it is a JavaScript program. To tell
the Web browser not to interpret the file, the mime type `text/plain`
is specified in the response option.

The charset encoding of the file is also provided using the `charset`
attribute.
