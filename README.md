# nlp-utils

A utility for doing common NLP tasks.

Currently, tests are used almost only for displaying output. Therefore the preferred way to run them is
to send the output to a file for viewing, e.g. 
% lein run > out.txt
% cat out.txt |more

NOTE: For some reason, leininingen 2.2.0 (and likely other versions than 2.0.0-preview10) crashes. Until I find out
why, I have been using the following  workaround:

1) Ensure you have at least leiningen 2.0.0 installed. Edit the lein script's version to use 2.0.0-preview10:
   ...
   LEIN_VERSION=export LEIN_VERSION="2.0.0-preview10"
   ..

   then run 
   %> lein self-install

2) From the latest lein script version (2.2.0 currently), comment out the line starting with LEIN_VERSION= 
and replace it with the following:


DEFAULT_LEIN_VERSION="2.2.0"
if [[ -n "$LEIN_VERSION" ]] ; then
    echo "Attempting to use pre-set lein version: $LEIN_VERSION"
else
    export LEIN_VERSION=$DEFAULT_LEIN_VERSION
fi

3) run the following command from the unix prompt (with surronding back ticks):
%> `./leinversion`

After this lein should work within 



