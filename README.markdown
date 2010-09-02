COME PADDLE BILLY
=================

It's a tool for comparing jars in case you ever wonder whether
you're preserving binary compatibility.  (You're not!)

Put exactly two scala distributions in dists/, like this:

    % ls -l dists/
    total 8
    -rw-r--r--  1 paulp  admin    0 Sep  1 16:31 PUT DISTS IN HERE.
    lrwxr-xr-x  1 paulp  admin   29 Sep  1 15:37 scala-2.8.0.final -> /scala/inst/scala-2.8.0.final
    drwxr-xr-x  8 paulp  admin  272 Sep  1 15:20 scala-2.8.0.r2-b20100901151019

And then "sbt run" and you're off to the races.

Sample output available in sample-output.txt.

STILL TODO
----------

Intelligence about what represents a breaking change.  For now, Billy leaves nothing out.